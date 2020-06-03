package com.apicatalog.jsonld.rdf;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;

public interface RdfSubject {

    boolean isIRI();
    boolean isBlankNode();
    
    IRI asIRI();
    
    BlankNode asBlankNode();
    
    @Override
    boolean equals(Object subject);
    
    @Override
    int hashCode();
}
