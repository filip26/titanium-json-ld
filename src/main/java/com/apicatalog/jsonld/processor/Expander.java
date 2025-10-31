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

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.ProcessingPolicy;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.api.CommonApi;
import com.apicatalog.jsonld.api.ContextApi;
import com.apicatalog.jsonld.api.LoaderApi;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.uri.UriUtils;

import jakarta.json.JsonStructure;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-expand">JsonLdProcessor.expand()</a>
 *
 */
public final class Expander implements
        CommonApi<Expander>,
        ContextApi<Expander>,
        LoaderApi<Expander> {

    private JsonLdOptions options;

    public Expander() {
        this(new JsonLdOptions());
    }

    public Expander(JsonLdOptions options) {
        this.options = options;
    }

    @Override
    public Expander options(JsonLdOptions options) {

        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    @Override
    public Expander context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

//    @Override
//    public Expander context(String contextLocation) {
//
//        URI contextUri = null;
//
//        if (contextLocation != null) {
//
//            contextUri = UriUtils.create(contextLocation);
//
//            if (contextUri == null) {
//                throw new IllegalArgumentException("Context location must be valid URI or null but is [" + contextLocation + ".");
//            }
//        }
//
//        return context(contextUri);
//    }

//    @Override
//    public Expander context(JsonStructure context) {
//        options.expandContext(context != null ? JsonDocument.of(context) : null);
//        return this;
//    }

    @Override
    public Expander context(Document context) {
        options.expandContext(context);
        return this;
    }

    @Override
    public Expander mode(JsonLdVersion processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public Expander base(URI baseUri) {
        options.base(baseUri);
        return this;
    }

    @Override
    public Expander loader(DocumentLoader loader) {
        options.loader(loader);
        return this;
    }

    @Override
    public Expander ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    /**
     * Get the result of the document expansion.
     *
     * @return {@link JsonArray} representing expanded document
     * @throws JsonLdException if the document expansion fails
     * @throws IOException
     */
//    public Collection<?> get() throws JsonLdException, IOException {
//        if (document != null) {
//            return Expander.expand(document, options);
//
//        } else if (documentUri != null) {
//            return Expander.expand(documentUri, options);
//        }
//        throw new IllegalStateException();
//    }

    /**
     * Experimental: Accept numeric @id. Disabled by default.
     *
     * @return builder instance
     */
    public Expander numericId() {
        options.setNumericId(true);
        return this;
    }

    /**
     * Experimental: Enables JSON-LD-STAR extension. Disabled by default.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     * @return builder instance
     */
    public Expander rdfStar() {
        options.setRdfStar(true);
        return this;
    }

    /**
     * Set a processing policy determining how to proceed when an undefined term is
     * found during an expansion. An unknown term is ignored by default.
     * 
     * @param policy a processing policy
     * @return builder instance
     */
    public Expander undefinedTermsPolicy(ProcessingPolicy policy) {
        options.setUndefinedTermsPolicy(policy);
        return this;
    }

    public static final Collection<?> expand(
            final Document document,
            final JsonLdOptions options) throws JsonLdException, IOException {
        return expand(document, false, options);
    }

    public static final Collection<?> expand(
            final PolyNode node,
            final Context context,
            final URI baseUrl,
            final JsonLdOptions options) throws JsonLdException, IOException {
        return expand(node, context, baseUrl, false, options);
    }
    
    public static final Collection<?> expandFrame(
            final Document document,
            final JsonLdOptions options) throws JsonLdException, IOException {
        return expand(document, true, options);
    }

    static final Context context(
            final URI document,
            final boolean frameExpansion,
            final JsonLdOptions options) throws JsonLdException, IOException {

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.
        URI baseUri = document;
        URI baseUrl = document;

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }

        if (options.getBase() != null) {
            baseUri = options.getBase();
        }

        var builder = new Context.Builder(
                baseUri,
                baseUrl,
                options.getProcessingMode())
                .loader(options.loader());

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {
            final var expandContext = options.getExpandContext().content();
//
//            if (contextValue.isPresent()) {
            builder.update(
                    expandContext.node(),
                    expandContext.adapter(),
                    baseUrl);
//            }
        }
//
//        // 7.
//        if (document.contextUrl() != null) {
//            builder.update(
//                    document.contextUrl().toString(),
//                    NativeAdapter.instance(),
//                    document.contextUrl());
//        }

        return  builder.build();
    }
    
    static final Collection<?> expand(
            final Document document,
            final boolean frameExpansion,
            final JsonLdOptions options) throws JsonLdException, IOException {

        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.
        URI baseUri = document.documentUrl();
        URI baseUrl = document.documentUrl();

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }

        if (options.getBase() != null) {
            baseUri = options.getBase();
        }

        var builder = new Context.Builder(
                baseUri,
                baseUrl,
                options.getProcessingMode())
                .loader(options.loader());

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {
            final var expandContext = options.getExpandContext().content();
//
//            if (contextValue.isPresent()) {
            builder.update(
                    expandContext.node(),
                    expandContext.adapter(),
                    baseUrl);
//            }
        }

        // 7.
        if (document.contextUrl() != null) {
            builder.update(
                    document.contextUrl().toString(),
                    NativeAdapter.instance(),
                    document.contextUrl());
        }

        return expand(
                document.content(),
                builder.build(),
                baseUrl,
                frameExpansion,
                options);
    }

    static final Collection<?> expand(
            final PolyNode node,
            final Context context,
            final URI baseUrl,
            boolean frameExpansion,
            final JsonLdOptions options) throws JsonLdException, IOException {

        final var runtime = ProcessingRuntime.of(options);

        // 8.
        var expanded = Expansion.expand(
                context,
                node.node(),
                node.adapter(),
                null,
                new Params(frameExpansion, false, baseUrl, runtime));

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

    public boolean ordered() {
        return options.isOrdered();
    }

    public void tick() {
        // TODO Auto-generated method stub

    }

    public DocumentLoader loader() {
        return options.loader();
    }

    public ProcessingPolicy undefinedTermPolicy() {
        return options.undefinedTermsPolicy();
    }

    public boolean isNumericId() {
        return options.isNumericId();
    }
}
