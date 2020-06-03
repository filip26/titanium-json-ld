package com.apicatalog.jsonld.rdf;

public interface RdfNQuad extends RdfTriple {

    String getGraphName();
    
    @Override
    int hashCode();

    @Override
    boolean equals(Object triple);
}
