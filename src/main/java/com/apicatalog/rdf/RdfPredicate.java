package com.apicatalog.rdf;

public interface RdfPredicate {

    enum Type { IRI, BLANK_NODE }
    
    boolean isIRI();
    boolean isBlankNode();

    
}
