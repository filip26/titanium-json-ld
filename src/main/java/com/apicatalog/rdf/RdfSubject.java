package com.apicatalog.rdf;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;

public interface RdfSubject {

    boolean isIRI();
    boolean isBlankNode();
    
    IRI asIRI();
    
    BlankNode asBlankNode();

}
