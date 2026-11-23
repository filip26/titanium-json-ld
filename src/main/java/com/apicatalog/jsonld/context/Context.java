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

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SequencedCollection;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderException;
import com.apicatalog.jsonld.runtime.Execution;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 */
public interface Context {

    default boolean isV11() {
        return version() == null
                ? true
                : Version.V1_1 == version();
    }

    default boolean isV10() {
        return version() == null
                ? false
                : Version.V1_0 == version();
    }

    /** context version, might be null if unspecified */
    Version version();

    Optional<TermDefinition> findTerm(final String value);

    Direction getDefaultBaseDirection();

    String getDefaultLanguage();

    URI getBaseUri();

    String getVocabularyMapping();

    Context getPreviousContext();

    void newTerm(
            String term,
            Map<String, ?> localContext,
            Map<String, Boolean> defined,
            DocumentLoader loader,
            final Execution runtime) throws JsonLdException;

    ContextBuilder newContext(DocumentLoader loader, final Execution runtime);

    InverseContext getInverseContext();

    void createInverseContext();

    String selectTerm(
            final Collection<String> preferredValues,
            final String variable,
            final Collection<String> containerMapping,
            final String typeLanguage);

    boolean containsTerm(final String term);

    Map<String, TermDefinition> getTermsMapping();

    // ---

    // TODO move to Frame.context()
    @Deprecated
    public static Object extract(Object document) throws JsonLdException {

        if (!(document instanceof Map map)) {
            throw new JsonLdException(ErrorCode.INVALID_CONTEXT_ENTRY,
                    "Document is not map but [" + document.getClass() + "].");
        }

        final var context = map.get(Keywords.CONTEXT);

        if (context != null) {
            return context;
        }

//        if (context != null
//                && (adapter.isString(context)
//                        || adapter.isCollection(context)
//                        || adapter.isMap(context))
//                && !adapter.isEmptyCollection(context)
//                && !adapter.isEmptyMap(context)) {
//            return new PolyNode(context, adapter);
//        }

        return Map.of();

    }

    public static Object unwrap(Object context) {

        Object node = context;
        boolean changed = false;

        if (node instanceof SequencedCollection<?> col) {
            if (col.size() == 1) {
                node = col.getFirst();
                changed = true;

            } else if (col.isEmpty()) {
                node = Map.of();
                changed = true;
            }
        }

        if (node instanceof Map map) {

            if (map.isEmpty()) {
                node = Map.of();
                changed = true;

            } else {
                final var ctx = map.get(Keywords.CONTEXT);
                if (ctx != null) {
                    node = ctx;
                    changed = true;
                }
            }
        }

        return changed
                ? node
                : context;
    }

    public static Map<String, ?> inject(
            final Map<String, ?> node,
            final Object context) throws JsonLdException {

        // 9.3.
        if (context != null
                && !(context instanceof Map map && map.isEmpty())
                && !(context instanceof Collection col && col.isEmpty())) {
            final var compacted = new LinkedHashMap<String, Object>(node.size() + 1);
            compacted.put(Keywords.CONTEXT, context);
            compacted.putAll(node);
            return compacted;

        }
        return node;
    }

    public static Document load(URI uri, DocumentLoader loader) throws JsonLdException {

        Document document = null;

        if (document == null) {

            final var loaderOptions = new DocumentLoader.Options(
                    false,
                    Terms.PROFILE_CONTEXT,
                    List.of(Terms.PROFILE_CONTEXT));

            try {

                document = loader.loadDocument(uri, loaderOptions);

                // 5.2.5.1.
            } catch (LoaderException e) {
                throw new JsonLdException(ErrorCode.LOADING_REMOTE_CONTEXT_FAILED,
                        "There was a problem encountered loading a remote context [" + uri + "]", e);
            }

            if (document == null) {
                throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.");
            }
        }

        // 5.2.5.2.
        if (document.content() == null || !(document.content() instanceof Map)) {
            throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT,
                    "Imported context is not valid JSON-LD context: " + document.content() + ".");
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

    public static class Builder {

        Context ctx;

        DocumentLoader loader;
        Execution runtime;

        public Builder(Version version) {
            this(null, null, version);
        }

        public Builder(URI base, Version version) {
            this(base, base, version);
        }

        public Builder(URI baseUri, URI baseUrl, Version version) {
            this.ctx = new ActiveContext(baseUri, baseUrl, version);
        }

        public Builder loader(DocumentLoader loader) {
            this.loader = loader;
            return this;
        }

        public Builder runtime(Execution runtime) {
            this.runtime = runtime;
            return this;
        }

        public Builder baseUri(URI base) {
            ctx.setBaseUri(base);
            return this;
        }

        public URI baseUri() {
            return ctx.getBaseUri();
        }

        // TODO better
        public Context build() throws JsonLdException {
//            var ctx = new ActiveContext(baseUri, baseUrl, runtime);
//            if (context != null) {
//                ctx = ctx.newContext()
//                        .create(context, adapter, baseUrl);
//            }
            return ctx;
        }

        public Builder update(Object node, boolean acceptInline, URI baseUrl)
                throws JsonLdException {
            // TODO merge if set
//            this.context = node;
//            this.adapter = adapter;
//            this.baseUrl = baseUrl;
            this.ctx = ctx.newContext(loader, runtime)
                    .useInline(acceptInline)
                    .build(node, baseUrl);
            return this;
        }

//        private final ActiveContext updateContext(
//                final ActiveContext activeContext,
//                final Object expandedContext,
//                final TreeAdapter adapter,
//                final URI baseUrl)
//                throws JsonLdException, IOException {
//
//            if (adapter.isCollection(expandedContext)) {
//
//                if (adapter.isSingleElement(expandedContext)) {
//
//                    var value = adapter.singleElement(expandedContext);
//
//                    if (adapter.isMap(value)) {
//
//                        var context = adapter.property(Keywords.CONTEXT, value);
//
//                        if (!adapter.isNull(context)) {
//                            return activeContext
//                                    .newContext(loader)
//                                    .build(context, adapter, baseUrl);
//                        }
//                    }
//                }
//
//                return activeContext.newContext(loader).build(expandedContext, adapter, baseUrl);
//
//            } else if (adapter.isMap(expandedContext)) {
//
//                var context = adapter.property(Keywords.CONTEXT, expandedContext);
//
//                if (!adapter.isNull(context)) {
//                    return activeContext
//                            .newContext(loader)
//                            .build(context, adapter, baseUrl);
//                }
//            }
//            return activeContext.newContext(loader).build(
//                    List.of(NativeMaterializer.node(expandedContext, adapter)),
//                    NativeAdapter.instance(),
//                    baseUrl);
//        }
//
    }

    @Deprecated
    void setBaseUri(URI base);
}
