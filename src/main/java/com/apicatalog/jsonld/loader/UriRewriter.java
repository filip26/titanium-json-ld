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

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;

public final class UriRewriter implements DocumentLoader {

    private final Map<URI, URI> rewrite;

    private final DocumentLoader loader;

    private UriRewriter(Map<URI, URI> rewrite, final DocumentLoader loader) {
        this.rewrite = rewrite;
        this.loader = loader;
    }

    /** Creates a builder. */
    public static final Builder newBuilder(DocumentLoader loader) {
        return new Builder(Map.of(), loader);
    }
    
    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        final var target = rewrite.get(url);

        if (target != null) {
            return loader.loadDocument(target, options);
        }

        return loader.loadDocument(url, options);
    }

    public static final class Builder {

        private final Map<URI, URI> rewrite;
        private final DocumentLoader loader;

        Builder(Map<URI, URI> resources, DocumentLoader loader) {
            this.rewrite = new LinkedHashMap<>(resources);
            this.loader = loader;
        }

        public Builder map(String from, String to) {
            return map(URI.create(from), URI.create(to));
        }

        public Builder map(URI from, URI to) {
            rewrite.put(from, to);
            return this;
        }

        /** Builds an immutable {@link UriRewriter} instance. */
        public UriRewriter build() {
            return new UriRewriter(Map.copyOf(rewrite), loader);
        }
    }
}