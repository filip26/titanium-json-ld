package com.apicatalog.rdf;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;

public interface RdfGraphName {

    boolean isIRI();
    boolean isBlankNode();
    
    IRI asIRI();
    
    BlankNode asBlankNode();

}
