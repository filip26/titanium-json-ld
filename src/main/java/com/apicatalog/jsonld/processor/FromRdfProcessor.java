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
package com.apicatalog.jsonld.processor;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.serialization.RdfToJsonld;

import jakarta.json.JsonArray;

public final class FromRdfProcessor {

    private FromRdfProcessor() {
    }

    public static final JsonArray fromRdf(final Document document, final JsonLdOptions options) throws JsonLdError {

        return RdfToJsonld
                    .with(document.getRdfContent().orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Expected RDF document but got [" + document.getContentType() + "]")))
                    .ordered(options.isOrdered())
                    .rdfDirection(options.getRdfDirection())
                    .useNativeTypes(options.isUseNativeTypes())
                    .useRdfType(options.isUseRdfType())
                    .processingMode(options.getProcessingMode())
                    .uriValidation(options.isUriValidation())
                    .build();
    }

    public static JsonArray fromRdf(URI documentUri, JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + documentUri + "].");
        }

        final Document remoteDocument =
                                options
                                    .getDocumentLoader()
                                    .loadDocument(documentUri,
                                            new DocumentLoaderOptions()
                                                    );

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return fromRdf(remoteDocument, options);
    }
}
