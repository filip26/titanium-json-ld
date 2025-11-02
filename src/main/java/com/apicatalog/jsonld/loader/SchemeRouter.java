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

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;

public final class SchemeRouter implements DocumentLoader {

    private final Map<String, DocumentLoader> loaders;
    private final DocumentLoader fallback;

    private SchemeRouter(final Map<String, DocumentLoader> loaders, DocumentLoader fallback) {
        this.loaders = loaders;
        this.fallback = fallback;
    }

    /** Creates a new builder for {@code SchemeRouter}. */
    public static Builder newBuilder() {
        return new Builder();
    }

    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        if (url == null) {
            throw new IllegalArgumentException("The url must not be null.");
        }

        final DocumentLoader loader = loaders.getOrDefault(url.getScheme().toLowerCase(), null);

        if (loader != null) {
            return loader.loadDocument(url, options);
        }

        if (fallback != null) {
            return fallback.loadDocument(url, options);
        }

        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "URL scheme [" + url.getScheme() + "] is not supported.");

    }

    // ---------- Builder ----------

    public static final class Builder {

        private final Map<String, DocumentLoader> routes = new LinkedHashMap<>();
        private DocumentLoader fallback;

        /** Routes the given URI scheme to the supplied loader. */
        public Builder route(String scheme, DocumentLoader loader) {
            routes.put(scheme, loader);
            return this;
        }

        /** Sets a fallback loader used when no scheme matches. */
        public Builder fallback(DocumentLoader loader) {
            this.fallback = loader;
            return this;
        }

        /** Builds the router. */
        public DocumentLoader build() {
            return new SchemeRouter(Map.copyOf(routes), fallback);
        }
    }
}
