package com.apicatalog.rdf.api;

/**
 * An optimized RDF quad consumer interface designed for high-speed processing
 * and seamless integration with third-party libraries.
 * 
 * This interface minimizes unnecessary object instantiation, enhancing
 * efficiency in RDF data processing.
 */
public interface RdfQuadConsumer {

    /**
     * Accepts an RDF quad where the object is an IRI or a blank node.
     * 
     * @param subject   The subject of the quad (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the quad (IRI); never {@code null}.
     * @param object    The object of the quad (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param graph     The graph (IRI or blank node identifier prefixed with "_:"),
     *                  or {@code null} for the default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException
     */
    RdfQuadConsumer quad(
            String subject,
            String predicate,
            String object,
            String graph) throws RdfConsumerException;

    /**
     * Accepts an RDF quad where the object is a literal with a datatype. Optimized
     * for efficient handling of typed literals.
     * 
     * @param subject   The subject of the quad (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the quad (IRI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param datatype  The datatype IRI of the literal; never {@code null}.
     * @param graph     The graph (IRI or blank node identifier prefixed with "_:"),
     *                  or {@code null} for the default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException
     */
    RdfQuadConsumer quad(
            String subject,
            String predicate,
            String literal,
            String datatype,
            String graph) throws RdfConsumerException;

    /**
     * Accepts an RDF quad where the object is a localized string value. Optimized
     * for efficient handling of language-tagged literals.
     * 
     * @param subject   The subject of the quad (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the quad (IRI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param language  The language tag of the literal; never {@code null}.
     * @param direction The text direction of the literal (optional, may be
     *                  {@code null}).
     * @param graph     The graph (IRI or blank node identifier prefixed with "_:"),
     *                  or {@code null} for the default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     * @throws RdfConsumerException
     */
    RdfQuadConsumer quad(
            String subject,
            String predicate,
            String literal,
            String language,
            String direction,
            String graph) throws RdfConsumerException;
}
