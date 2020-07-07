package com.apicatalog.rdf;

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
