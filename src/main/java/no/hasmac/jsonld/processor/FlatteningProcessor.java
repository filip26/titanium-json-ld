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
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.flattening.Flattening;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;

import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class FlatteningProcessor {

    private FlatteningProcessor() {
    }

    public static JsonStructure flatten(final URI input, final URI context, final JsonLdOptions options) throws JsonLdError {

        if (context == null) {
            return flatten(input, (Document)null, options);
        }

        assertDocumentLoader(options, input);

        final Document contextDocument = options.getDocumentLoader().loadDocument(context, new DocumentLoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }

        return flatten(input, contextDocument, options);
    }

    public static JsonStructure flatten(final Document input, final URI context, final JsonLdOptions options) throws JsonLdError {

        if (context == null) {
            return flatten(input, (Document)null, options);
        }

        assertDocumentLoader(options, context);

        final Document contextDocument = options.getDocumentLoader().loadDocument(context, new DocumentLoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }

        return flatten(input, contextDocument, options);
    }

    public static JsonStructure flatten(final URI input, final Document context, final JsonLdOptions options) throws JsonLdError {

        assertDocumentLoader(options, input);

        final DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return flatten(remoteDocument, context, options);
    }

    public static JsonStructure flatten(final Document input, final Document context, final JsonLdOptions options) throws JsonLdError {

        // 4.
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);

        final JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);

        // 5.
        // 6.
        JsonStructure flattenedOutput = Flattening.with(expandedInput).ordered(options.isOrdered()).flatten();

        // 6.1.
        if (context != null) {

            final Document document = JsonDocument.of(MediaType.JSON_LD, flattenedOutput);

            JsonLdOptions compactionOptions = new JsonLdOptions(options);

            if (options.getBase() != null) {
                compactionOptions.setBase(options.getBase());

            } else if (options.isCompactArrays()) {
                compactionOptions.setBase(input.getDocumentUrl());
            }

            flattenedOutput = CompactionProcessor.compact(document, context, compactionOptions);
        }

        return flattenedOutput;
    }

    private static void assertDocumentLoader(final JsonLdOptions options, final URI target) throws JsonLdError {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + target + "].");
        }
    }

}
