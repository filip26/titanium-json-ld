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

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.flattening.Flattening;
import com.apicatalog.jsonld.loader.LoaderOptions;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Flattener {

    private Flattener() {
    }

    public static final Object flatten(final URI input, final URI context, final JsonLdOptions options, final Execution runtime) throws JsonLdException, IOException {

        if (context == null) {
            return flatten(input, (Document) null, options, runtime);
        }

        assertDocumentLoader(options, input);

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        var remoteDocument = options.loader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return flatten(remoteDocument, context, options, runtime);

//        assertDocumentLoader(options, input);
//
//        var contextDocument = options.getDocumentLoader().loadDocument(context, new LoaderOptions());
//
//        if (contextDocument == null) {
//            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
//        }
//
//        return flatten(input, contextDocument, options);
    }

    public static final Object flatten(
            final URI input,
            final Document context,
            final JsonLdOptions options,
            final Execution runtime) throws JsonLdException, IOException {

        assertDocumentLoader(options, input);

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        var remoteDocument = options.loader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        return flatten(remoteDocument, context, options, runtime);
    }

    public static final Object flatten(
            final Document input,
            final URI context,
            final JsonLdOptions options,
            final Execution runtime) throws JsonLdException, IOException {

        if (context == null) {
            return flatten(input, (Document) null, options, runtime);
        }

        assertDocumentLoader(options, context);

        final var contextDocument = Context.load(options.loader(), context);

        return flatten(input, contextDocument, options, runtime);
    }

    public static final Object flatten(
            final Document input,
            final Document context,
            final JsonLdOptions options,
            final Execution runtime) throws JsonLdException, IOException {

        // 4.
        final var expansionOptions = JsonLdOptions.copyOf(options).ordered(false);

        var expandedInput = Expander.expand(input, expansionOptions, runtime);

        // 6.
        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());

        // 6.1.
        if (context != null && context.content() != null) {

            JsonLdOptions compactionOptions = JsonLdOptions.copyOf(options);

            if (options.base() != null) {
                compactionOptions.base(options.base());

            } else if (options.isCompactArrays()) {
                compactionOptions.base(input.documentUrl());
            }

            flattenedOutput = Compactor.compact(
                    flattenedOutput,
                    input.documentUrl(),
                    context.content(),
                    compactionOptions,
                    runtime);
        }

        return flattenedOutput;
    }

    public static final Object flatten(
            final Document input,
            final Context context,
            final JsonLdOptions options,
            final Execution runtime) throws JsonLdException, IOException {

        // 4.
        final var expansionOptions = JsonLdOptions.copyOf(options).ordered(false);

        var expandedInput = Expander.expand(input, expansionOptions, runtime);

        // 6.
        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());

        // 6.1.
        if (context != null) {

            JsonLdOptions compactionOptions = JsonLdOptions.copyOf(options);

            if (options.base() != null) {
                compactionOptions.base(options.base());

            } else if (options.isCompactArrays()) {
                compactionOptions.base(input.documentUrl());
            }

            flattenedOutput = Compactor.compact(
                    flattenedOutput,
                    context,
                    compactionOptions,
                    runtime);
        }

        return flattenedOutput;
    }

    private static final void assertDocumentLoader(final JsonLdOptions options, final URI target) throws JsonLdException {
        if (options.loader() == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + target + "].");
        }
    }

}
