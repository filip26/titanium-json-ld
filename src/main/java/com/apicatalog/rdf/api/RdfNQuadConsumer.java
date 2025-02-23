package com.apicatalog.rdf.api;

/**
 * Consumes N-Quads.
 */
public interface RdfNQuadConsumer {

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
     */
    RdfNQuadConsumer nquad(
            String subject,
            String predicate,
            String object,
            String graph);

    /**
     * Accepts an RDF quad where the object is an N-Quad literal.
     * 
     * @param subject   The subject of the quad (IRI or blank node identifier
     *                  prefixed with "_:"); never {@code null}.
     * @param predicate The predicate of the quad (IRI); never {@code null}.
     * @param literal   The literal value of the object; never {@code null}.
     * @param datatype  The datatype IRI of the literal; never {@code null}.
     * @param langTag   The well-formed language tag of the literal; never {@code null}.
     * @param graph     The graph (IRI or blank node identifier prefixed with "_:"),
     *                  or {@code null} for the default graph.
     * 
     * @return An instance enabling fluent programming; never {@code null}.
     */
    RdfNQuadConsumer nquad(
            String subject,
            String predicate,
            String literal,
            String datatype,
            String langTag,
            String graph);
}
