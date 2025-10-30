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
package com.apicatalog.jsonld.api;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.rdf.model.RdfDataset;

import jakarta.json.JsonStructure;

public class MockLoader implements DocumentLoader {

    private final JsonStructure structure;
    private final RdfDataset dataset;

    public MockLoader(final JsonStructure response) {
        this.structure = response;
        this.dataset = null;
    }

    public MockLoader(final RdfDataset dataset) {
        this.structure = null;
        this.dataset = dataset;
    }

    @Override
    public Document loadDocument(URI url, LoaderOptions options) throws JsonLdException {

        if (structure != null) {
            final Document remoteDocument = JsonDocument.of(structure);
            remoteDocument.setDocumentUrl(url);

            return remoteDocument;
        }


        throw new IllegalStateException();
    }
}
