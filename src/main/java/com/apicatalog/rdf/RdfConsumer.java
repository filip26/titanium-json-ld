package com.apicatalog.rdf;

/**
 * RDF dataset consumer interface designed for speed, streamlined processing,
 * and efficiency.
 * 
 * This interface minimizes unnecessary object instantiation and facilitates
 * seamless integration with third-party libraries, enabling efficient RDF data
 * processing.
 */
public interface RdfConsumer {

    /**
     * Sets the default graph as the active scope. Invoked when triples belong to
     * the unnamed, default graph.
     */
    void defaultGraph();

    /**
     * Sets a named graph as the active scope. Ensures that subsequent triples are
     * associated with the specified graph.
     * 
     * @param graph      The name of the graph (IRI or blank node identifier
     *                   prefixed with "_:").
     * @param blankGraph {@code true} if the graph name is a blank node identifier.
     */
    void namedGraph(String graph, boolean blankGraph);

    /**
     * Accepts a new RDF triple where the object is an IRI or a blank node. The
     * triple is processed within the currently active graph scope.
     * 
     * @param subject        The subject of the triple (IRI or blank node identifier
     *                       prefixed with "_:").
     * @param blankSubject   {@code true} if the subject is a blank node identifier.
     * @param predicate      The predicate of the triple (IRI or blank node
     *                       identifier prefixed with "_:").
     * @param blankPredicate {@code true} if the predicate is a blank node
     *                       identifier.
     * @param object         The object of the triple (IRI or blank node identifier
     *                       prefixed with "_:").
     * @param blankObject    {@code true} if the object is a blank node identifier.
     */
    void triple(
            String subject,
            boolean blankSubject,
            String predicate,
            boolean blankPredicate,
            String object,
            boolean blankObject);

    /**
     * Accepts a new RDF triple where the object is a literal value. The triple is
     * processed within the currently active graph scope. Optimized for efficient
     * handling of typed and language-tagged literals.
     * 
     * @param subject        The subject of the triple (IRI or blank node identifier
     *                       prefixed with "_:").
     * @param blankSubject   {@code true} if the subject is a blank node identifier.
     * @param predicate      The predicate of the triple (IRI or blank node
     *                       identifier prefixed with "_:").
     * @param blankPredicate {@code true} if the predicate is a blank node
     *                       identifier.
     * @param literal        The literal value of the object.
     * @param datatype       The datatype IRI of the literal (never {@code null}).
     */
    void triple(
            String subject,
            boolean blankSubject,
            String predicate,
            boolean blankPredicate,
            String literal,
            String datatype);

    /**
     * Accepts a new RDF triple where the object is a literal value. The triple is
     * processed within the currently active graph scope. Optimized for efficient
     * handling of typed and language-tagged literals.
     * 
     * @param subject        The subject of the triple (IRI or blank node identifier
     *                       prefixed with "_:").
     * @param blankSubject   {@code true} if the subject is a blank node identifier.
     * @param predicate      The predicate of the triple (IRI or blank node
     *                       identifier prefixed with "_:").
     * @param blankPredicate {@code true} if the predicate is a blank node
     *                       identifier.
     * @param literal        The literal value of the object.
     * @param language       The language code of the literal (never {@code null}).
     * @param direction      The direction of the literal (optional, may be
     *                       {@code null}).
     */
    void triple(
            String subject,
            boolean blankSubject,
            String predicate,
            boolean blankPredicate,
            String literal,
            String language,
            String direction);
}
