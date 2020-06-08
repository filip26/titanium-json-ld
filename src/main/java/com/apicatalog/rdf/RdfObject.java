package com.apicatalog.rdf;

public interface RdfObject {

    enum Type { IRI, BLANK_NODE, LITERAL }
    
    boolean isLiteral();
    
    boolean isIRI();
    
    boolean isBlankNode();
    
    RdfLiteral getLiteral();
        
}
