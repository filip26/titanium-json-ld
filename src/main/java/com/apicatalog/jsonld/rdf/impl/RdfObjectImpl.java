package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfObject;

final class RdfObjectImpl implements RdfObject {

    private final RdfLiteral literal;
    private final String string;
    
    protected RdfObjectImpl(RdfLiteral literal) {
        this.literal = literal;
        this.string = null;
    }

    protected RdfObjectImpl(String string) {
        this.string = string;
        this.literal = null;
    }

    @Override
    public boolean isLiteral() {
        return literal != null;
    }

    @Override
    public boolean isString() {
        return string != null;
    }

    @Override
    public RdfLiteral asLiteral() {
        return literal;
    }

    @Override
    public String asString() {
        return string;
    }

    
}
