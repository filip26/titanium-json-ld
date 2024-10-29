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

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.serialization.RdfToJsonld;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.JsonArray;

public final class FromRdfProcessor {

    private FromRdfProcessor() {
    }

    public static final JsonArray fromRdf(final RdfDocument document, final JsonLdOptions options) throws JsonLdError {
        return fromRdf(document.getRdfContent()
                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                        "Expected RDF document but got [mediaType=" + document.getContentType() + ", uri=" + document.getDocumentUrl() + "]")),
                options);
    }

    public static final JsonArray fromRdf(final RdfDataset dataset, final JsonLdOptions options) throws JsonLdError {
        return RdfToJsonld
                .with(dataset)
                .ordered(options.isOrdered())
                .rdfDirection(options.getRdfDirection())
                .useNativeTypes(options.isUseNativeTypes())
                .useRdfType(options.isUseRdfType())
                .processingMode(options.getProcessingMode())
                .uriValidation(options.isUriValidation())
                .build();
    }
}
