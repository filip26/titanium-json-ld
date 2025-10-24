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
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-expand">JsonLdProcessor.expand()</a>
 *
 */
public final class Expander {

    Expander() {
    }

    public static final Collection<?> expand(final URI input, final JsonLdOptions options) throws JsonLdError, IOException {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + input + "].");
        }

        var remoteDocument = options.getDocumentLoader().loadDocument(
                input,
                new LoaderOptions().setExtractAllScripts(options.isExtractAllScripts()));

        if (remoteDocument != null && remoteDocument.getContent() instanceof PolyNode) {
            @SuppressWarnings("unchecked")
            final Document<PolyNode> remote = remoteDocument;
            return expand(remote, options);
        }

        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
    }

    public static final Collection<?> expand(Document<PolyNode> input, final JsonLdOptions options) throws JsonLdError, IOException {
        if (input == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "RemoteDocument is null.");
        }

        return expand(
                input.getContent(),
                input.getContextUrl(),
                input.getDocumentUrl(),
                options,
                false);
    }

    public static final Collection<?> expand(
            final PolyNode node,
            final URI contextUrl,
            final URI documentUrl,
            final JsonLdOptions options,
            boolean frameExpansion) throws JsonLdError, IOException {

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.
        URI baseUri = null;
        URI baseUrl = null;

        if (documentUrl != null) {
            baseUrl = documentUrl;
            baseUri = baseUrl;
        }

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }

        if (options.getBase() != null) {
            baseUri = options.getBase();
        }

        final var runtime = ProcessingRuntime.of(options);

        var contextBuilder = new Context.Builder(
                baseUri,
                baseUrl,
                options.getProcessingMode())
                .loader(options.getDocumentLoader());

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {

            final var contextValue = options.getExpandContext().getJsonContent();

            if (contextValue.isPresent()) {
                contextBuilder.update(contextValue.get(), JakartaAdapter.instance(), baseUrl);
            }
        }

        // 7.
        if (contextUrl != null) {
            contextBuilder.update(
                    contextUrl.toString(),
                    NativeAdapter.instance(),
                    contextUrl);
        }

        // 8.
        var expanded = Expansion.expand(
                contextBuilder.build(),
                node.node(),
                node.adapter(),
                null,
                new Params(frameExpansion, options.isOrdered(), false, baseUrl, runtime));

        // 8.1
        if (expanded instanceof Map map
                && map.size() == 1
                && map.containsKey(Keywords.GRAPH)) {
            expanded = map.get(Keywords.GRAPH);
        }

        // 8.2
        if (expanded == null) {
            return Set.of();
        }

        if (expanded instanceof Collection<?> collection) {
            return collection;
        }

        // 8.3
        return Set.of(expanded);
    }
}
