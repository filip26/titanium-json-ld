package com.apicatalog.rdf.impl;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfObject;

final class RdfObjectImpl implements RdfObject {

    private final RdfLiteral literal;
    private final IRI iri;
    private final BlankNode blankNode;
    
    private RdfObjectImpl(RdfLiteral literal, IRI iri, BlankNode blankNode) {
        this.literal = literal;
        this.iri = iri;
        this.blankNode = blankNode;
    }

    protected static final RdfObject of(RdfLiteral literal) {
        if (literal == null) {
            throw new IllegalArgumentException();
        }
        return new RdfObjectImpl(literal, null, null);
    }

    protected static final RdfObject of(IRI iri) {
        if (iri == null) {
            throw new IllegalArgumentException();
        }
        return new RdfObjectImpl(null, iri, null);
    }

    protected static final RdfObject of(BlankNode blankNode) {
        if (blankNode == null) {
            throw new IllegalArgumentException();
        }
        return new RdfObjectImpl(null, null, blankNode);
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
