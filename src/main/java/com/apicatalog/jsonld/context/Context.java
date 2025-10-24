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
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.compaction.UriCompaction;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.processor.ProcessingRuntime;
import com.apicatalog.tree.io.NodeAdapter;

import jakarta.json.JsonValue;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 *
 */
public interface Context {

    Optional<TermDefinition> findTerm(final String value);

    Direction getDefaultBaseDirection();

    String getDefaultLanguage();

    URI getBaseUri();

    String getVocabularyMapping();

    Context getPreviousContext();

    ProcessingRuntime runtime();

    TermDefinitionBuilder newTerm(Object localContext, NodeAdapter adapter, Map<String, Boolean> defined);

    ActiveContextBuilder newContext();

    // ---

    UriExpansion uriExpansion();

    Map<String, ?> expandValue(String property, Object value, NodeAdapter adapter) throws JsonLdError, IOException;

    /* compaction */
//    UriCompaction uriCompaction();

    String compactUri(String variable) throws JsonLdError;
    
    String compactUriWithVocab(String variable) throws JsonLdError;

    Object compactValue(Map<String, ?> value, String property) throws JsonLdError;

    InverseContext getInverseContext();

    void createInverseContext();

    Optional<String> selectTerm(
            final Collection<String> preferredValues,
            final String variable,
            final Collection<String> containerMapping,
            final String typeLanguage);

    boolean containsTerm(final String term);

    Map<String, TermDefinition> getTermsMapping();

    static class Bx {

//        void Bx of() {
//            
//            
//            PolyNode context = null;
//
//            if (adapter.keys(node).contains(Keywords.CONTEXT)) {
//                var contextNode = adapter.property(Keywords.CONTEXT, node);
//                if ((adapter.isString(contextNode)
//                        || adapter.isCollection(contextNode)
//                        || adapter.isMap(contextNode))
//                        && !adapter.isEmptyCollection(contextNode)
//                        && !adapter.isEmptyMap(contextNode)) {
//                    context = new PolyNode(contextNode, adapter);
//                }
//            }
//        }

    }

    static class Builder {

        URI baseUri;
        URI baseUrl;
        ProcessingRuntime runtime;

        Object context;
        NodeAdapter adapter;

        Context ctx;

        // TODO remove runtime
        public Builder(URI base, ProcessingRuntime runtime) {
            this(base, base, runtime);
        }

        public Builder(URI baseUri, URI baseUrl, ProcessingRuntime runtime) {
            this.baseUri = baseUri;
            this.baseUrl = baseUrl;
            this.runtime = runtime;
            this.ctx = new ActiveContext(baseUri, baseUrl, runtime);
        }

        // TODO better
        public Context build() throws JsonLdError, IOException {
//            var ctx = new ActiveContext(baseUri, baseUrl, runtime);
//            if (context != null) {
//                ctx = ctx.newContext()
//                        .create(context, adapter, baseUrl);
//            }
            return ctx;
        }

        public void update(Object node, NodeAdapter adapter, URI baseUrl) throws JsonLdError, IOException {
            // TODO merge if set
            this.context = node;
            this.adapter = adapter;
            this.baseUrl = baseUrl;
            this.ctx = ctx.newContext().build(node, adapter, baseUrl);
        }

        private static final ActiveContext updateContext(final ActiveContext activeContext, final Object expandedContext, final NodeAdapter adapter, final URI baseUrl)
                throws JsonLdError, IOException {

            if (adapter.isCollection(expandedContext)) {

                if (adapter.isSingleElement(expandedContext)) {

                    var value = adapter.singleElement(expandedContext);

                    if (adapter.isMap(value)) {

                        var context = adapter.property(Keywords.CONTEXT, value);

                        if (!adapter.isNull(context)) {
                            return activeContext
                                    .newContext()
                                    .build(context, adapter, baseUrl);
                        }
                    }
                }

                return activeContext.newContext().build(expandedContext, adapter, baseUrl);

            } else if (adapter.isMap(expandedContext)) {

                var context = adapter.property(Keywords.CONTEXT, expandedContext);

                if (!adapter.isNull(context)) {
                    return activeContext
                            .newContext()
                            .build(context, adapter, baseUrl);
                }
            }
            return activeContext.newContext().build(
                    JsonProvider.instance().createArrayBuilder().add((JsonValue) expandedContext).build(), adapter, baseUrl);
        }

    }

    // ---
//  void createInverseContext();

//    URI getBaseUrl();

//  boolean containsTerm(final String term);

//  boolean containsProtectedTerm();

//    URI getBaseUrl();

    // ---
}
