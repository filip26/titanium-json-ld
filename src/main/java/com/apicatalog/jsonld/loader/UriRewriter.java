/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;

public final class UriRewriter implements DocumentLoader {

    private final Map<URI, URI> rewrite;

    private final Map<String, String> rebase;

    private final DocumentLoader loader;

    private UriRewriter(
            Map<URI, URI> rewrite,
            Map<String, String> rebase,
            DocumentLoader loader) {
        this.rewrite = rewrite;
        this.rebase = rebase;
        this.loader = loader;
    }

    /** Creates a builder. */
    public static final Builder newBuilder(DocumentLoader loader) {
        return new Builder(Map.of(), Map.of(), loader);
    }

    public static final Builder copyOf(UriRewriter rewriter) {
        return new Builder(rewriter.rewrite, rewriter.rebase, rewriter.loader);
    }

    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        final var target = rewrite.get(url);

        if (target != null) {

            var document = loader.loadDocument(target, options);

            if (document != null) {
                // update URL
                if (target.equals(document.url())) {
                    return Document.of(
                            document.content(),
                            document.contentType(),
                            document.profile(),
                            url,
                            document.context());
                }
                return document;
            }
        }

        Entry<String, String> base = null;

        for (var entry : rebase.entrySet()) {
            if (url.toString().startsWith(entry.getKey())) {
                base = entry;
                break;
            }
        }

        if (base != null) {

            final var relativeTarget = url.toString().substring(base.getKey().length());

            var document = loader.loadDocument(URI.create(base.getValue() + relativeTarget), options);

            // update URL
            if (document != null && document.url() != null && document.url().toString().startsWith(base.getValue())) {

                final var relativeSource = document.url().toString().substring(base.getValue().length());

                return Document.of(
                        document.content(),
                        document.contentType(),
                        document.profile(),
                        URI.create(base.getKey() + relativeSource),
                        document.context());
            }

            return document;
        }

        return loader.loadDocument(url, options);
    }

    public static final class Builder {

        private final Map<URI, URI> rewrite;
        private final Map<String, String> rebase;
        private DocumentLoader loader;

        Builder(Map<URI, URI> resources, Map<String, String> rebase, DocumentLoader loader) {
            this.rewrite = new LinkedHashMap<>(resources);
            this.rebase = new LinkedHashMap<>(rebase);
            this.loader = loader;
        }

        public Builder rewrite(String from, String to) {
            return rewrite(URI.create(from), URI.create(to));
        }

        public Builder rewrite(URI from, URI to) {
            rewrite.put(
                    Objects.requireNonNull(from),
                    Objects.requireNonNull(to));
            return this;
        }

        public Builder rebase(String from, String to) {
            rebase.put(
                    Objects.requireNonNull(from),
                    Objects.requireNonNull(to));
            return this;
        }

        public Builder loader(DocumentLoader loader) {
            this.loader = Objects.requireNonNull(loader);
            return this;
        }

        /** Builds an immutable {@link UriRewriter} instance. */
        public UriRewriter build() {
            return new UriRewriter(
                    Map.copyOf(rewrite),
                    Map.copyOf(rebase),
                    loader);
        }
    }
}