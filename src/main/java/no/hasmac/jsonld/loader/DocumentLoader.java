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

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.document.Document;

/**
 * The {@link DocumentLoader} defines an interface that custom document
 * loaders have to implement to be used to retrieve remote documents and
 * contexts.
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#loaddocumentcallback">LoadDocumentCallback
 *      Specification</a>
 *
 */
public interface DocumentLoader {

    /**
     * Retrieve a remote document.
     *
     * @param url of the remote document to fetch
     * @param options to set the behavior of the loader
     * @return {@link Document} representing a remote document
     * @throws JsonLdError
     */
    Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError;
}
