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
package no.hasmac.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.document.Document;

public final class SchemeRouter implements DocumentLoader {

    private static final DocumentLoader INSTANCE =
                                new SchemeRouter()
                                        .set("http", HttpLoader.defaultInstance())
                                        .set("https", HttpLoader.defaultInstance())
                                        .set("file", new FileLoader());

    private final Map<String, DocumentLoader> loaders;

    public SchemeRouter() {
        this.loaders = new LinkedHashMap<>();
    }

    public static DocumentLoader defaultInstance() {
        return INSTANCE;
    }

    public SchemeRouter set(final String scheme, final DocumentLoader loader) {
        loaders.put(scheme, loader);
        return this;
    }

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        if (url == null) {
            throw new IllegalArgumentException("The url must not be null.");
        }

        final DocumentLoader loader = loaders.getOrDefault(url.getScheme().toLowerCase(), null);

        if (loader == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "URL scheme [" + url.getScheme() + "] is not supported.");
        }

        return loader.loadDocument(url, options);
    }

}
