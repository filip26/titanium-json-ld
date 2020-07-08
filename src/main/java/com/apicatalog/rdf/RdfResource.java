package com.apicatalog.rdf;

/**
 * An immutable RDF statement's value. Represents an absolute IRI or blank node identifier.
 */
public interface RdfResource extends RdfValue {

    @Override
    default boolean isLiteral() {
        return false;
    }
 
    @Override
    default RdfLiteral asLiteral() {
        throw new ClassCastException();
    }
}
