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
package no.hasmac.jsonld.api;

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.document.RdfDocument;
import no.hasmac.jsonld.loader.DocumentLoader;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;
import no.hasmac.rdf.RdfDataset;

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
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        if (structure != null) {
            final Document remoteDocument = JsonDocument.of(structure);
            remoteDocument.setDocumentUrl(url);

            return remoteDocument;
        }

        if (dataset != null) {
            final Document remoteDocument = RdfDocument.of(dataset);
            remoteDocument.setDocumentUrl(url);

            return remoteDocument;
        }

        throw new IllegalStateException();
    }



}
