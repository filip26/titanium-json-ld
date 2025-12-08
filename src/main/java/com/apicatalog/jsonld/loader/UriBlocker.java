/*
 * Copyright 2025 the original author or authors.
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
import java.util.LinkedHashSet;
import java.util.Set;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.LoaderException.ErrorCode;

public final class UriBlocker implements DocumentLoader {

    private final Set<URI> uris;

    private final Set<String> bases;

    private final DocumentLoader loader;

    private final boolean blacklist;

    private UriBlocker(
            boolean blacklist,
            Set<URI> uris,
            Set<String> bases,
            DocumentLoader loader) {
        this.blacklist = blacklist;
        this.uris = uris;
        this.bases = bases;
        this.loader = loader;
    }

    public static Builder newWhitelist(DocumentLoader loader) {
        return new Builder(loader, false);
    }

    public static Builder newBlacklist(DocumentLoader loader) {
        return new Builder(loader, true);
    }

    @Override
    public Document loadDocument(URI uri, Options options) throws LoaderException {

        var match = uris.contains(uri);

        if (!match && !bases.isEmpty()) {

            final var uriString = uri.toString();

            for (final var base : bases) {

                match = uriString.startsWith(base);

                if (match) {
                    break;
                }
            }
        }

        if (blacklist == match) {
            throw new LoaderException(
                    ErrorCode.BLOCKED_URI,
                    uri,
                    blacklist
                            ? "The URI [" + uri + "] is black-listed"
                            : "The URI [" + uri + "] is not white-listed");
        }

        return loader.loadDocument(uri, options);
    }

    public static class Builder {

        private final DocumentLoader loader;
        private final boolean blacklist;
        private final Set<URI> uris;
        private final Set<String> bases;

        private Builder(DocumentLoader loader, boolean blacklist) {
            this.loader = loader;
            this.blacklist = blacklist;
            this.uris = new LinkedHashSet<URI>();
            this.bases = new LinkedHashSet<String>();
        }

        public Builder uri(URI uri) {
            uris.add(uri);
            return this;
        }

        public Builder base(String base) {
            bases.add(base);
            return this;
        }

        public UriBlocker build() {
            return new UriBlocker(
                    blacklist,
                    Set.copyOf(uris),
                    Set.copyOf(bases),
                    loader);
        }
    }
}
