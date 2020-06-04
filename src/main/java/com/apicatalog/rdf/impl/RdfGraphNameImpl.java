package com.apicatalog.rdf.impl;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfGraphName;

class RdfGraphNameImpl implements RdfGraphName {

    private final IRI iri;
    private final BlankNode blankNode;
    
    protected RdfGraphNameImpl(IRI iri) {
        this.iri = iri;
        this.blankNode = null;
    }
    
    protected RdfGraphNameImpl(BlankNode blankNode) {
        this.blankNode = blankNode;
        this.iri = null;
    }

    @Override
    public boolean isIRI() {
        return iri != null;
    }

    @Override
    public boolean isBlankNode() {
        return blankNode != null;
    }

    @Override
    public IRI asIRI() {
        return iri;
    }

    @Override
    public BlankNode asBlankNode() {
        return blankNode;
    }
    
    @Override
    public String toString() {
        return isIRI() ? iri.toString() : blankNode.toString();
    }
}
