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
package no.hasmac.jsonld.processor;

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.deseralization.JsonLdToRdf;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.flattening.NodeMap;
import no.hasmac.jsonld.flattening.NodeMapBuilder;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;
import no.hasmac.rdf.Rdf;
import no.hasmac.rdf.RdfDataset;

import jakarta.json.JsonArray;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class ToRdfProcessor {

    private ToRdfProcessor() {
    }

    public static RdfDataset toRdf(final URI input, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return toRdf(remoteDocument, options);
    }

    public static RdfDataset toRdf(Document input, final JsonLdOptions options) throws JsonLdError {

        final JsonLdOptions expansionOptions = new JsonLdOptions(options);

        expansionOptions.setProcessingMode(options.getProcessingMode());
        expansionOptions.setBase(options.getBase());
        expansionOptions.setExpandContext(options.getExpandContext());

        final JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);

        return JsonLdToRdf
                        .with(
                            NodeMapBuilder.with(expandedInput, new NodeMap()).build(),
                            Rdf.createDataset()
                            )
                        .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                        .rdfDirection(options.getRdfDirection())
                        .uriValidation(options.isUriValidation())
                        .build();
    }
}
