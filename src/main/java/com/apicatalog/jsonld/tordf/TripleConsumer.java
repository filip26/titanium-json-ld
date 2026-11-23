/*
 * Copyright 2020-2026 the original author or authors.
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
interface TripleConsumer {

    /**
     * Sets the default graph as the active scope. This method is invoked when
     * triples belong to the unnamed default graph.
     */
    void defaultGraph();

    /**
     * Sets a named graph as the active scope. Ensures that subsequent triples are
     * associated with the specified graph.
     * 
     * @param graph The name of the graph (URI or blank node identifier prefixed
     *              with "_:"); never {@code null}.
     */
    void namedGraph(String graph);

    /**
     * Accepts an RDF triple where the object is an URI or a blank node. The triple
     * is processed within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (URI); never {@code null}.
     * @param object    The object of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     */
    void triple(
            String subject,
            String predicate,
            String object);

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
     */
    void triple(
            String subject,
            String predicate,
            String literal,
            String datatype);

    /**
     * Accepts an RDF triple where the object is a localized string value. Optimized
     * for efficient handling of language-tagged literals. The triple is processed
     * within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (URI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (URI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param datatype  The datatype URI of the literal; never {@code null}.
     * @param language  The language tag of the literal; (optional, may be
     *                  {@code null}).
     * @param direction The text direction of the literal (optional, may be
     *                  {@code null}).
     */
    void triple(
            String subject,
            String predicate,
            String literal,
            String datatype,
            String language,
            String direction);
}
