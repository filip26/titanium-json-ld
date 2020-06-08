package com.apicatalog.rdf;

public interface RdfGraphName {

    enum Type { IRI, BLANK_NODE }
    
    boolean isIRI();
    boolean isBlankNode();
    
}
