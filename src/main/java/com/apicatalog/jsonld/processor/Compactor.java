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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.java.NativeMaterializer3;

import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class Compactor {

    private Compactor() {
    }

    public static final Map<String, ?> compact(final URI input, final URI context, final JsonLdOptions options) throws JsonLdError, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + context + "].");
        }

        final Document contextDocument = options.getDocumentLoader().loadDocument(context, new LoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }

        return compact(input, contextDocument, options);
    }

    public static final Map<String, ?> compact(final URI input, final Document context, final JsonLdOptions options) throws JsonLdError, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        final LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setExtractAllScripts(options.isExtractAllScripts());

        final Document remoteDocument = options.getDocumentLoader().loadDocument(input, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Returned document is null [" + input + "].");
        }

        return compact(remoteDocument, context, options);
    }

    public static final Map<String, ?> compact(final Document<PolyNode> input, final URI context, final JsonLdOptions options) throws JsonLdError, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + context + "].");
        }

        final Document<?> contextDocument = options.getDocumentLoader().loadDocument(context, new LoaderOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Returned context is null [" + context + "] is null.");
        }

        return compact(input, contextDocument, options);
    }

    public static final Map<String, ?> compact(final Document<PolyNode> input, final Document<?> context, final JsonLdOptions options) throws JsonLdError, IOException {

        // 4.
        final var expansionOptions = new JsonLdOptions(options)
                .setOrdered(false)
                .setExtractAllScripts(false);

        final var expandedInput = Expander.expand(input, expansionOptions, false);

//new Visitor().root(expandedInput, NativeAdapter.instance()).traverse(
//        
//        v -> {
//            System.out.println(v.node() + ", " + v.nodeType() + ", " + v.nodeContext() + ", " + v.node().getClass());
//        }
//        
//        );
//        var m = new JakartaMaterializer().node(expandedInput, NativeAdapter.instance());
//System.out.println(m);
        // 5.
        URI contextBase = input.getDocumentUrl();

        if (contextBase == null) {
            contextBase = options.getBase();
        }

        // 6.
        final JsonValue contextValue = context.getJsonContent()
                .map(ctx -> JsonUtils.flatten(ctx, Keywords.CONTEXT))
                .orElse(JsonValue.EMPTY_JSON_OBJECT);

        // 7.
        final var activeContext = new ActiveContext(ProcessingRuntime.of(options))
                .newContext()
                .build(contextValue, JakartaAdapter.instance(), contextBase);

        // 8.
        if (activeContext.getBaseUri() == null) {

            if (options.getBase() != null) {
                activeContext.setBaseUri(options.getBase());

            } else if (options.isCompactToRelative()) {
                activeContext.setBaseUri(input.getDocumentUrl());
            }
        }

        // 9.
        var compactedOutput = Compaction
                .with(activeContext)
                .compactArrays(options.isCompactArrays())
                .ordered(options.isOrdered())
                .compact(expandedInput);
//System.out.println("CMPOUT < " + compactedOutput);
        // 9.1.
        if (compactedOutput instanceof Collection<?> col) {
            if (col.isEmpty()) {
                compactedOutput = Map.of();

            } else {
                // 9.2.
                compactedOutput = Map.of(
                        activeContext.compactUriWithVocab(Keywords.GRAPH),
//                        .uriCompaction().vocab(true).compact(Keywords.GRAPH),
                        compactedOutput);
            }
        }

        if (compactedOutput == null
                /*|| compactedOutput instanceof Map map && map.isEmpty()*/) {
            return Map.of();
        }

        // 9.3.
        if (JsonUtils.isNotNull(contextValue)
                && !JsonUtils.isEmptyArray(contextValue)
                && !JsonUtils.isEmptyObject(contextValue)) {
//            compactedOutput = JsonProvider.instance().createObjectBuilder((Map)compactedOutput)
//                    .add(Keywords.CONTEXT, contextValue)
//                    .build();
            var compacted = new LinkedHashMap<String, Object>(((Map) compactedOutput).size());
            compacted.put(Keywords.CONTEXT, new NativeMaterializer3().node(contextValue, JakartaAdapter.instance()));
            compacted.putAll((Map) compactedOutput);
            return compacted;
        }

        return (Map) compactedOutput;
    }
}
