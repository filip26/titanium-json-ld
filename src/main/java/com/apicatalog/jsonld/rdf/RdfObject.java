package com.apicatalog.jsonld.rdf;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;

public interface RdfObject {

    boolean isLiteral();
    
    boolean isIRI();
    
    boolean isBlankNode();
    
    RdfLiteral asLiteral();
    
    IRI asIRI();
    
    BlankNode asBlankNode();
    
    @Override
    boolean equals(Object object);
    
    @Override
    int hashCode();

}
