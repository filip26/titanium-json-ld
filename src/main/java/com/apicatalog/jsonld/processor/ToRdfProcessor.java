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

import java.io.IOException;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.jsonld.tordf.ToRdf;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.java.NativeAdapter;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class ToRdfProcessor {

    private ToRdfProcessor() {
    }

    public static final void toRdf(final RdfQuadConsumer consumer, final URI input, final JsonLdOptions options) throws JsonLdError, IOException {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        toRdf(consumer, remoteDocument, options);
    }

    public static final void toRdf(final RdfQuadConsumer consumer, final Document input, final JsonLdOptions options) throws JsonLdError, IOException {
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);

        expansionOptions.setProcessingMode(options.getProcessingMode());
        expansionOptions.setBase(options.getBase());
        expansionOptions.setExpandContext(options.getExpandContext());

        final var expandedInput = Expander.expand(input, false, expansionOptions);

        toRdf(consumer, expandedInput, options);
    }

    public static final void toRdf(final RdfQuadConsumer consumer, final Object expandedInput, final JsonLdOptions options) throws JsonLdError {
        ToRdf
                .with(new NodeMapBuilder(expandedInput, new NodeMap()).build())
                .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                .rdfDirection(options.getRdfDirection())
                .uriValidation(options.getUriValidation())
                .provide(consumer);
    }
}
