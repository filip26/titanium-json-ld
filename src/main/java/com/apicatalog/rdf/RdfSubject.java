package com.apicatalog.rdf;

public interface RdfSubject {

    enum Type { IRI, BLANK_NODE }
    
    boolean isIRI();
    boolean isBlankNode();
    
    @Override
    String toString();
}
