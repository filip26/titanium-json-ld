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
package com.apicatalog.jsonld.context;

import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.java.NativeAdapter;

import jakarta.json.JsonValue;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 *
 */
public interface Context {

    default boolean isV11() {
        return version() == null
                ? true
                : JsonLdVersion.V1_1 == version();
    }

    default boolean isV10() {
        return version() == null
                ? false
                : JsonLdVersion.V1_0 == version();
    }

    /** context version, might be null if unspecified */
    JsonLdVersion version();

    PolyNode source();

    Optional<TermDefinition> findTerm(final String value);

    Direction getDefaultBaseDirection();

    String getDefaultLanguage();

    URI getBaseUri();

    String getVocabularyMapping();

    Context getPreviousContext();

    @Deprecated
    TermDefinitionBuilder newTerm(Object localContext, NodeAdapter adapter, Map<String, Boolean> defined, DocumentLoader loader);

    @Deprecated
    ContextBuilder newContext(DocumentLoader loader);

    // ---

    @Deprecated
    UriExpansion uriExpansion(DocumentLoader loader);

    InverseContext getInverseContext();

    void createInverseContext();

    Optional<String> selectTerm(
            final Collection<String> preferredValues,
            final String variable,
            final Collection<String> containerMapping,
            final String typeLanguage);

    boolean containsTerm(final String term);

    Map<String, TermDefinition> getTermsMapping();

    public static PolyNode extract(PolyNode document) throws JsonLdException {

        final var node = document.node();
        final var adapter = document.adapter();

        if (!adapter.isMap(node)) {
            throw new JsonLdException(JsonLdErrorCode.INVALID_CONTEXT_ENTRY, "Document is not map but [" + node + "].");
        }

        final var context = adapter.property(Keywords.CONTEXT, node);

        if (context != null) {
            return new PolyNode(context, adapter);
        }

//        if (context != null
//                && (adapter.isString(context)
//                        || adapter.isCollection(context)
//                        || adapter.isMap(context))
//                && !adapter.isEmptyCollection(context)
//                && !adapter.isEmptyMap(context)) {
//            return new PolyNode(context, adapter);
//        }

        return new PolyNode(Map.of(), NativeAdapter.instance());

    }

    public static PolyNode unwrap(PolyNode context) {

        Object node = context.node();
        var adapter = context.adapter();

        boolean changed = false;

        if (adapter.isSingleElement(node)) {
            node = adapter.singleElement(node);
            changed = true;
        }

        if (adapter.isMap(node)) {
            final var ctx = adapter.property(Keywords.CONTEXT, node);
            if (ctx != null) {
                node = ctx;
                changed = true;
            }
        }

        if (node == null || adapter.isNull(node) || adapter.isEmpty(node)) {
            node = Map.of();
            adapter = NativeAdapter.instance();
            changed = true;
        }

        return changed
                ? new PolyNode(node, adapter)
                : context;
    }

    public static Map<String, ?> inject(
            final Map<String, ?> node,
            final PolyNode context) throws JsonLdException, IOException {

        // 9.3.
        if (!PolyNode.isEmptyOrNull(context)) {
            final var compacted = new LinkedHashMap<String, Object>(node.size() + 1);
            compacted.put(Keywords.CONTEXT, context);
            compacted.putAll(node);
            return compacted;

        }
        return node;
    }

    public static Document load(DocumentLoader loader, URI uri) throws JsonLdException, IOException {

        Document document = null;

        if (document == null) {

            final var loaderOptions = new LoaderOptions();
            loaderOptions.setProfile(ProfileConstants.CONTEXT);
            loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

            try {

                document = loader.loadDocument(uri, loaderOptions);

                // 5.2.5.1.
            } catch (JsonLdException e) {
                throw new JsonLdException(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "There was a problem encountered loading a remote context [" + uri + "]", e);
            }

            if (document == null) {
                throw new JsonLdException(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.");
            }
        }

        // 5.2.5.2.
        if (!PolyNode.isMap(document.content())) {
            throw new JsonLdException(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is not valid JSON-LD context: " + document.content() + ".");
        }

        return document;
    }

//    
//    // 5.2.5.3.
//    final var contextNode = imported.property(Keywords.CONTEXT);
//
//    if(contextNode==null)
//    {
//        throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context does not contain @context key and is not valid JSON-LD context.");
//    }
//
////        var newContext = new NativeMaterializer3().node(contextNode, imported.adapter());
////
////        // remove @base from a remote context
////        if (newContext instanceof Map map && map.containsKey(Keywords.BASE)) {
////            @SuppressWarnings("unchecked")
////            var hashMap = new HashMap<>(map);
////            hashMap.remove(Keywords.BASE);
////            newContext = hashMap;
////        }
//
//// 5.2.6
//    try
//    {
//        ctx = ctx
//                .newContext(loader)
////                    .remoteContexts(new ArrayList<>(remoteContexts))
////                    .validateScopedContext(validateScopedContext)
//                .build(contextNode, imported.adapter(), document.getDocumentUrl());
////            .build(newContext, NativeAdapter.instance(), document.getDocumentUrl());                
////
////
//        return this;
//
//    }catch(
//    JsonLdError e)
//    {
//        throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
//    }
    // FIXME
//        return null;
//}

    static class Builder {

//        Object context;
//        NodeAdapter adapter;

        ActiveContext ctx;

        DocumentLoader loader;

        public Builder(JsonLdVersion version) {
            this(null, null, version);
        }

        public Builder baseUri(URI base) {
            ctx.setBaseUri(base);
            return this;
        }

        public URI baseUri() {
            return ctx.getBaseUri();
        }

        public Builder(URI base, JsonLdVersion version) {
            this(base, base, version);
        }

        public Builder(URI baseUri, URI baseUrl, JsonLdVersion version) {
//            this.baseUri = baseUri;
//            this.baseUrl = baseUrl;
//            this.version = version;
            this.ctx = new ActiveContext(baseUri, baseUrl, version);
        }

        // TODO better
        public Context build() throws JsonLdException, IOException {
//            var ctx = new ActiveContext(baseUri, baseUrl, runtime);
//            if (context != null) {
//                ctx = ctx.newContext()
//                        .create(context, adapter, baseUrl);
//            }
            return ctx;
        }

        public Builder update(PolyNode node, URI baseUrl) throws JsonLdException, IOException {
            return update(node.node(), node.adapter(), baseUrl);
        }

        public Builder update(Object node, NodeAdapter adapter, URI baseUrl) throws JsonLdException, IOException {
            // TODO merge if set
//            this.context = node;
//            this.adapter = adapter;
//            this.baseUrl = baseUrl;
            this.ctx = ctx.newContext(loader).build(node, adapter, baseUrl);
            return this;
        }

        private final ActiveContext updateContext(
                final ActiveContext activeContext,
                final Object expandedContext,
                final NodeAdapter adapter,
                final URI baseUrl)
                throws JsonLdException, IOException {

            if (adapter.isCollection(expandedContext)) {

                if (adapter.isSingleElement(expandedContext)) {

                    var value = adapter.singleElement(expandedContext);

                    if (adapter.isMap(value)) {

                        var context = adapter.property(Keywords.CONTEXT, value);

                        if (!adapter.isNull(context)) {
                            return activeContext
                                    .newContext(loader)
                                    .build(context, adapter, baseUrl);
                        }
                    }
                }

                return activeContext.newContext(loader).build(expandedContext, adapter, baseUrl);

            } else if (adapter.isMap(expandedContext)) {

                var context = adapter.property(Keywords.CONTEXT, expandedContext);

                if (!adapter.isNull(context)) {
                    return activeContext
                            .newContext(loader)
                            .build(context, adapter, baseUrl);
                }
            }
            return activeContext.newContext(loader).build(
                    JsonProvider.instance().createArrayBuilder().add((JsonValue) expandedContext).build(), adapter, baseUrl);
        }

        public Builder loader(DocumentLoader loader) {
            this.loader = loader;
            return this;
        }
    }

//
//    static class Bx {
//
////        void Bx of() {
////            
////            
////            PolyNode context = null;
////
////            if (adapter.keys(node).contains(Keywords.CONTEXT)) {
////                var contextNode = adapter.property(Keywords.CONTEXT, node);
////                if ((adapter.isString(contextNode)
////                        || adapter.isCollection(contextNode)
////                        || adapter.isMap(contextNode))
////                        && !adapter.isEmptyCollection(contextNode)
////                        && !adapter.isEmptyMap(contextNode)) {
////                    context = new PolyNode(contextNode, adapter);
////                }
////            }
////        }
//    }

//    PolyNode asNode();

// ---
//  void createInverseContext();

//    URI getBaseUrl();

//  boolean containsTerm(final String term);

//  boolean containsProtectedTerm();

//    URI getBaseUrl();

// ---
}
