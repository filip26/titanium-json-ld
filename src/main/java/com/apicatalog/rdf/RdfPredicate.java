package com.apicatalog.rdf;

public interface RdfPredicate {

    enum Type { IRI, BLANK_NODE }
    
    boolean isIRI();
    boolean isBlankNode();
    
    @Override
    String toString();
    
    @Override
    int hashCode();
    
    @Override
    boolean equals(Object obj);
}
