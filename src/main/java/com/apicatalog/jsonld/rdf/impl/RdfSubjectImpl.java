package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.rdf.RdfSubject;

class RdfSubjectImpl implements RdfSubject {

    private final IRI iri;
    private final BlankNode blankNode;
    
    protected RdfSubjectImpl(IRI iri) {
        this.iri = iri;
        this.blankNode = null;
    }
    
    protected RdfSubjectImpl(BlankNode blankNode) {
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
