package com.apicatalog.jsonld.rdf;

public interface RdfObject {

    boolean isLiteral();
    
    boolean isString();
    
    RdfLiteral asLiteral();
    
    String asString();
}
