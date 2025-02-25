package com.apicatalog.rdf.api;

/**
 * An RDF triple consumer interface designed for high-speed processing and
 * seamless integration with third-party libraries.
 * 
 * Intended to be used in cases where triples are emitted in bulk as a set
 * belonging to the same graph, and where a producer keeps triple sets associated
 * with graphs.
 * 
 * This interface minimizes unnecessary object instantiation, enhancing
 * efficiency in RDF data processing.
 */
public interface RdfTripleConsumer {

    /**
     * Sets the default graph as the active scope. This method is invoked when
     * triples belong to the unnamed default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     */
    RdfTripleConsumer defaultGraph();

    /**
     * Sets a named graph as the active scope. Ensures that subsequent triples are
     * associated with the specified graph.
     * 
     * @param graph The name of the graph (IRI or blank node identifier prefixed
     *              with "_:"); never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     */
    RdfTripleConsumer namedGraph(String graph);

    /**
     * Accepts an RDF triple where the object is an IRI or a blank node. The triple
     * is processed within the currently active graph scope.
     * 
     * @param subject   The subject of the triple (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (IRI); never {@code null}.
     * @param object    The object of the triple (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException 
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
     * @param subject   The subject of the triple (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (IRI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param datatype  The datatype IRI of the literal; never {@code null}.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException 
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
     * @param subject   The subject of the triple (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the triple (IRI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param language  The language tag of the literal; never {@code null}.
     * @param direction The text direction of the literal (optional, may be
     *                  {@code null}).
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException 
     */
    RdfTripleConsumer triple(
            String subject,
            String predicate,
            String literal,
            String language,
            String direction) throws RdfConsumerException;
}
