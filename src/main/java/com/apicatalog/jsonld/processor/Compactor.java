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
import java.util.Map;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.compaction.UriCompaction;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.expansion.Expander;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.PolyNode;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Compactor {

    private Compactor() {
    }

    public static final Map<String, ?> compact(final URI input, final URI context, final JsonLdOptions options) throws JsonLdException, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Returned document is null [" + input + "].");
        }

        return compact(remoteDocument, context, options);
    }

    public static final Map<String, ?> compact(final URI input, final Document context, final JsonLdOptions options) throws JsonLdException, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Returned document is null [" + input + "].");
        }

        return compact(remoteDocument, context, options);
    }

    public static final Map<String, ?> compact(
            final Document input,
            final URI contextUri,
            final JsonLdOptions options) throws JsonLdException, IOException {

        URI contextBase = input.documentUrl();

        if (contextBase == null) {
            contextBase = options.getBase();
        }

        // 6.
//        final JsonValue contextValue = context.getJsonContent()
//                .map(ctx -> JsonUtils.flatten(ctx, Keywords.CONTEXT))
//                .orElse(JsonValue.EMPTY_JSON_OBJECT);

        final var expandedInput = Expander.expand(
                input,
                new JsonLdOptions(options)
                        .setOrdered(false)
                        .setExtractAllScripts(false));

        return compact(
                expandedInput,
                input.documentUrl(),
                Context.load(options.getDocumentLoader(), contextUri).content(),
                options);
    }

    public static final Map<String, ?> compact(
            final Document input,
            final Document context,
            final JsonLdOptions options) throws JsonLdException, IOException {
//        
//        URI contextBase = input.getDocumentUrl();
//
//        if (contextBase == null) {
//            contextBase = options.getBase();
//        }
//
//        // 6.
////        final JsonValue contextValue = context.getJsonContent()
////                .map(ctx -> JsonUtils.flatten(ctx, Keywords.CONTEXT))
////                .orElse(JsonValue.EMPTY_JSON_OBJECT);
//
//        // 7.
//        final var activeContext = new ActiveContext(ProcessingRuntime.of(options))
//                .newContext()
//                .build(context.asNode(), contextBase);
//
//        // 8.
//        if (activeContext.getBaseUri() == null) {
//
//            if (options.getBase() != null) {
//                activeContext.setBaseUri(options.getBase());
//
//            } else if (options.isCompactToRelative()) {
//                activeContext.setBaseUri(input.getDocumentUrl());
//            }
//        }

//        final var ctx = Context.unwrap(context.getContent());
//        context(ctx, input.getDocumentUrl(), options)

        final var expandedInput = Expander.expand(input, new JsonLdOptions(options)
                .setOrdered(false).setExtractAllScripts(false));

        return compact(
                expandedInput,
                input.documentUrl(),
                context.content(),
                options);

    }

    public static final Map<String, ?> compact(
            final Object expanded,
            final URI baseUrl,
            final PolyNode context,
            final JsonLdOptions options) throws JsonLdException, IOException {

        URI contextBase = baseUrl;

        if (contextBase == null) {
            contextBase = options.getBase();
        }

        final var localContext = Context.unwrap(context);

        // 6.
//        final JsonValue contextValue = context.getJsonContent()
//                .map(ctx -> JsonUtils.flatten(ctx, Keywords.CONTEXT))
//                .orElse(JsonValue.EMPTY_JSON_OBJECT);

        // 7.
        final var builder = new Context.Builder(options.getProcessingMode())
                .loader(options.getDocumentLoader())
                .update(localContext, contextBase);

        // 8.
        if (builder.baseUri() == null) {

            if (options.getBase() != null) {
                builder.baseUri(options.getBase());

            } else if (options.isCompactToRelative()) {
                builder.baseUri(baseUrl);
            }
        }

        return compact(
                expanded,
                builder.build(),
                options);
    }

    static final Map<String, ?> compact(
            final Object expanded,
            final Context context,
            final JsonLdOptions options) throws JsonLdException, IOException {

        var activeContext = context;

//        // 8.
//        if (builder.baseUri() == null) {

//            if (options.getBase() != null) {
//        activeContext = new Context.Builder(context)
//                .baseUri(options.getBase())
//                .build();

//                builder.baseUri(options.getBase());
//
//            } else if (options.isCompactToRelative()) {
//                builder.baseUri(baseUrl);
//            }
//        }
//
        final var runtime = new ProcessingRuntime(options);

        // 9.
        var compactedOutput = Compaction
                .with(activeContext, runtime)
                .compactArrays(options.isCompactArrays())
                .ordered(options.isOrdered())
                .compact(expanded);

        // 9.1.
        if (compactedOutput instanceof Collection<?> col) {

            if (col.isEmpty()) {
                return Map.of();

            } else if (!PolyNode.isEmptyOrNull(context.source())) {
                return Map.of(
                        Keywords.CONTEXT,
                        context.source(),
                        UriCompaction.withVocab(context, Keywords.GRAPH),
                        compactedOutput);
                
            } else {
                return Map.of(
                        UriCompaction.withVocab(context, Keywords.GRAPH),
                        compactedOutput);
            }
        }

        if (compactedOutput == null || compactedOutput instanceof Map map && map.isEmpty()) {
            return Map.of();
        }

        if (compactedOutput instanceof Map map) {
            @SuppressWarnings("unchecked")
            final var typedMap = (Map<String, ?>) map;

            if (context.source() != null) {
                return Context.inject(typedMap, context.source());
            }

            return typedMap;
        }

        throw new IllegalStateException();
    }
}
