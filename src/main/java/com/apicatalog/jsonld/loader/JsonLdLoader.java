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

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;

/**
 * The {@link JsonLdLoader} defines an interface that custom loaders have to
 * implement to be used to retrieve remote JSON-LD documents and contexts.
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#loaddocumentcallback">LoadDocumentCallback
 *      Specification</a>
 *
 */
public interface JsonLdLoader {

    /**
     * Retrieve a remote document.
     *
     * @param url     of the remote document to fetch
     * @param options to set the behavior of the loader
     * @return {@link Document} representing a remote document
     * @throws JsonLdError if the document loading fails
     */
    Document loadDocument(URI url, LoaderOptions options) throws JsonLdError;
}
