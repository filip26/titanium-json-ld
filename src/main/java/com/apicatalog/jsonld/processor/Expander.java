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
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.expansion.Expansion.Config;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.NativeAdapter;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.JsonStructure;

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
            return expand(remote, options, false);
        }

        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
    }

    public static final Collection<?> expand(Document<PolyNode> input, final JsonLdOptions options, boolean frameExpansion) throws JsonLdError, IOException {

        if (input == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "RemoteDocument is null.");
        }

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.
        URI baseUri = null;
        URI baseUrl = null;

        if (input.getDocumentUrl() != null) {
            baseUrl = input.getDocumentUrl();
            baseUri = baseUrl;
        }

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }

        if (options.getBase() != null) {
            baseUri = options.getBase();
        }

        var contextBuilder = new Context.Builder(
                baseUri,
                baseUrl,
                ProcessingRuntime.of(options));

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {

            @SuppressWarnings("unchecked")
            final Optional<JsonStructure> contextValue = options.getExpandContext().getJsonContent();

            if (contextValue.isPresent()) {
                contextBuilder.update(contextValue.get(), JakartaAdapter.instance(), baseUrl);
            }
        }

        // 7.
        if (input.getContextUrl() != null) {
            contextBuilder.update(
                    input.getContextUrl().toString(),
                    NativeAdapter.instance(),
                    input.getContextUrl());
        }

        var content = input.getContent();

        // 8.
        var expanded = Expansion
                .with(new Config(frameExpansion, options.isOrdered(), false))
                .expand(
                        contextBuilder.build(),
                        content.node(),
                        content.adapter(),
                        null,
                        baseUrl);

        // 8.1
        if (expanded instanceof Map object
                && object.size() == 1
                && object.containsKey(Keywords.GRAPH)) {
            expanded = object.get(Keywords.GRAPH);
        }

        // 8.2
        if (expanded == null) {
            return Collections.emptySet();
        }

        if (expanded instanceof Collection<?> collection) {
            return collection;
        }

        // 8.3
        return Set.of(expanded);
    }
}
