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
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.PolyDocument;
import com.apicatalog.jsonld.flattening.Flattening;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.java.NativeAdapter;

import jakarta.json.JsonStructure;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Flattener {

    private Flattener() {
    }

    public static final Object flatten(final URI input, final URI context, final JsonLdOptions options) throws JsonLdError, IOException {

        if (context == null) {
            return flatten(input, (Document<PolyNode>) null, options);
        }

        assertDocumentLoader(options, input);

        var contextDocument = options.getDocumentLoader().loadDocument(context, new LoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }

        return flatten(input, contextDocument, options);
    }

    public static final Object flatten(final Document<PolyNode> input, final URI context, final JsonLdOptions options) throws JsonLdError, IOException {

        if (context == null) {
            return flatten(input, (Document<PolyNode>) null, options);
        }

        assertDocumentLoader(options, context);

        var contextDocument = options.getDocumentLoader().loadDocument(context, new LoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }

        return flatten(input, contextDocument, options);
    }

    public static final Object flatten(final URI input, final Document<PolyNode> context, final JsonLdOptions options) throws JsonLdError, IOException {

        assertDocumentLoader(options, input);

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        var remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return flatten(remoteDocument, context, options);
    }

    public static final Object flatten(final Document<PolyNode> input, final Document<PolyNode> context, final JsonLdOptions options) throws JsonLdError, IOException {

        // 4.
        final var expansionOptions = new JsonLdOptions(options).setOrdered(false);

        var expandedInput = Expander.expand(input, expansionOptions);

        // 6.
        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());
//System.out.println("FLOUT < " + flattenedOutput);
        // 6.1.
        if (context != null) {

            //FIXME
//            var y = new JakartaMaterializer().node(flattenedOutput, NativeAdapter.instance());
//
//            var document = JsonDocument.of(MediaType.JSON_LD, (JsonStructure)y);
            
//            var document = PolyDocument.of(new PolyNode(flattenedOutput, NativeAdapter.instance()));

            JsonLdOptions compactionOptions = new JsonLdOptions(options);

            if (options.getBase() != null) {
                compactionOptions.setBase(options.getBase());

            } else if (options.isCompactArrays()) {
                compactionOptions.setBase(input.getDocumentUrl());
            }

            flattenedOutput = Compactor.compact(
                    flattenedOutput,
                    input.getDocumentUrl(),
                    context, 
                    compactionOptions);
        }

        return flattenedOutput;
    }

    private static final void assertDocumentLoader(final JsonLdOptions options, final URI target) throws JsonLdError {
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + target + "].");
        }
    }

}
