package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfObject;

final class RdfObjectImpl implements RdfObject {

    private final RdfLiteral literal;
    private final IRI iri;
    private final BlankNode blankNode;
    
    protected RdfObjectImpl(RdfLiteral literal) {
        this.literal = literal;
        this.iri = null;
        this.blankNode = null;
    }

    protected RdfObjectImpl(IRI iri) {
        this.iri = iri;
        this.literal = null;
        this.blankNode = null;
    }

    protected RdfObjectImpl(BlankNode blankNode) {
        this.iri = null;
        this.literal = null;
        this.blankNode = blankNode;
    }
    
    @Override
    public boolean isLiteral() {
        return literal != null;
    }

    @Override
    public boolean isIRI() {
        return iri != null;
    }

    @Override
    public RdfLiteral asLiteral() {
        return literal;
    }

    @Override
    public IRI asIRI() {
        return iri;
    }

    @Override
    public boolean isBlankNode() {
        return blankNode != null;
    }

    @Override
    public BlankNode asBlankNode() {
        return blankNode;
    }    
}
