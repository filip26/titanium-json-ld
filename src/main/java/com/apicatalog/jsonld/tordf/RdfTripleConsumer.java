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
package com.apicatalog.jsonld.tordf;

import com.apicatalog.rdf.api.RdfConsumerException;

/**
 * An RDF triple consumer interface designed for high-speed processing and
 * seamless integration with third-party libraries.
 * 
 * Intended to be used in cases where triples are emitted in bulk as a set
 * belonging to the same graph, and where a producer keeps triple sets
 * associated with graphs.
 * 
 * This interface minimizes unnecessary object instantiation, enhancing
 * efficiency in RDF data processing.
 */
interface RdfTripleConsumer {

    /**
     * Sets the default graph as the active scope. This method is invoked when
     * triples belong to the unnamed default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * 
     * @throws RdfConsumerException if an error occurs while processing the N-Quad
     *                              statement
     */
    RdfTripleConsumer defaultGraph() throws RdfConsumerException;

    /**
     * Sets a named graph as the active scope. Ensures that subsequent triples are
     * associated with the specified graph.
     * 
     * @param graph The name of the graph (URI or blank node identifier prefixed
     *              with "_:"); never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * 
     * @throws RdfConsumerException if an error occurs while processing the N-Quad
     *                              statement
     */
    RdfTripleConsumer namedGraph(String graph) throws RdfConsumerException;

    /**
     * Accepts an RDF triple where the object is an URI or a blank node. The triple
     * is processed within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (URI); never {@code null}.
     * @param object    The object of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * 
     * @throws RdfConsumerException if an error occurs while processing the N-Quad
     *                              statement
     */
    RdfTripleConsumer triple(
            String subject,
            String predicate,
            String object) throws RdfConsumerException;

    /**
     * Accepts an RDF triple where the object is a literal with a datatype.
     * Optimized for efficient handling of typed literals. The triple is processed
     * within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (URI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param datatype  The datatype URI of the literal; never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * 
     * @throws RdfConsumerException if an error occurs while processing the N-Quad
     *                              statement
     */
    RdfTripleConsumer triple(
            String subject,
            String predicate,
            String literal,
            String datatype) throws RdfConsumerException;

    /**
     * Accepts an RDF triple where the object is a localized string value. Optimized
     * for efficient handling of language-tagged literals. The triple is processed
     * within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (URI); never {@code null}. 
     * @param literal   The literal value of the object; never {@code null}.
     * @param language  The language tag of the literal; never {@code null}.
     * @param direction The text direction of the literal (optional, may be
     *                  {@code null}).
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * 
     * @throws RdfConsumerException if an error occurs while processing the N-Quad
     *                              statement
     */
    RdfTripleConsumer triple(
            String subject,
            String predicate,
            String literal,
            String language,
            String direction) throws RdfConsumerException;
}
