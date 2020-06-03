package com.apicatalog.rdf;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;

public interface RdfObject {

    boolean isLiteral();
    
    boolean isIRI();
    
    boolean isBlankNode();
    
    RdfLiteral asLiteral();
    
    IRI asIRI();
    
    BlankNode asBlankNode();
    

}
