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
import java.util.Collection;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.flattening.Flattening;
import com.apicatalog.tree.io.TreeIO;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Flattener {

    private Flattener() {
    }

    public static Object flatten(Document document, TreeIO context, Options options, Execution runtime) throws JsonLdException, IOException {
//        if (options.expandContext() == null) {
//            return flatten(input, (Document) null, options, runtime);
//        }

        // 4.
        final var expansionOptions = Options.copyOf(options).ordered(false);

        var expandedInput = Expander.expand(document, expansionOptions, runtime);
        System.out.println(document.content().node());
System.out.println(expandedInput);
        return flatten(
                expandedInput,
                document.url(),
                context,
                options,
                runtime);
    }

    public static Object flatten(TreeIO document, TreeIO context, Options options, Execution runtime) throws JsonLdException, IOException {
//        if (options.expandContext() == null) {
//            return flatten(input, (Document) null, options, runtime);
//        }

        // 4.
        final var expansionOptions = Options.copyOf(options).ordered(false);

        var expandedInput = Expander.expand(
                document,
                Expander.context(
                        null,
                        null,
                        options),
                null,
                expansionOptions,
                runtime);

//        final var contextDocument = Context.load(options.loader(), context);

        return flatten(
                expandedInput,
                null,
                context,
                options,
                runtime);
    }

    static final Object flatten(
            final Collection<?> expandedInput,
            final URI documentUrl,
            final TreeIO context,
            final Options options,
            final Execution runtime) throws JsonLdException, IOException {

        // 6.
        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());

        // 6.1.
        if (context != null) {

            Options compactionOptions = Options.copyOf(options);

            if (options.base() != null) {
                compactionOptions.base(options.base());

            } else if (options.isCompactArrays() && documentUrl != null) {
                compactionOptions.base(documentUrl);
            }

            flattenedOutput = Compactor.compact(
                    flattenedOutput,
                    documentUrl,
                    context,
                    compactionOptions,
                    runtime);
        }
        return flattenedOutput;
    }

    /* --- remove --- */
//    public static final Object flatten(
//            final Document input,
//            final Document context,
//            final Options options,
//            final Execution runtime) throws JsonLdException, IOException {
//
//        // 4.
//        final var expansionOptions = Options.copyOf(options).ordered(false);
//
//        var expandedInput = Expander.expand(input, expansionOptions, runtime);
//
//        // 6.
//        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());
//
//        // 6.1.
//        if (context != null && context.content() != null) {
//
//            Options compactionOptions = Options.copyOf(options);
//
//            if (options.base() != null) {
//                compactionOptions.base(options.base());
//
//            } else if (options.isCompactArrays()) {
//                compactionOptions.base(input.url());
//            }
//
//            flattenedOutput = Compactor.compact(
//                    flattenedOutput,
//                    input.url(),
//                    context.content(),
//                    compactionOptions,
//                    runtime);
//        }
//
//        return flattenedOutput;
//    }

//    public static final Object flatten(
//            final Document input,
//            final Context context,
//            final Options options,
//            final Execution runtime) throws JsonLdException, IOException {
//
//        // 4.
//        final var expansionOptions = Options.copyOf(options).ordered(false);
//
//        var expandedInput = Expander.expand(input, expansionOptions, runtime);
//
//        // 6.
//        var flattenedOutput = Flattening.flatten(expandedInput, options.isOrdered());
//
//        // 6.1.
//        if (context != null) {
//
//            Options compactionOptions = Options.copyOf(options);
//
//            if (options.base() != null) {
//                compactionOptions.base(options.base());
//
//            } else if (options.isCompactArrays()) {
//                compactionOptions.base(input.url());
//            }
//
//            flattenedOutput = Compactor.compact(
//                    flattenedOutput,
//                    context,
//                    compactionOptions,
//                    runtime);
//        }
//
//        return flattenedOutput;
//    }
//
//    private static final void assertDocumentLoader(final Options options, final URI target) throws JsonLdException {
//        if (options.loader() == null) {
//            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + target + "].");
//        }
//    }
}
