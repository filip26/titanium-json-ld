package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.rdf.RdfLiteral;
import com.apicatalog.jsonld.rdf.RdfObject;

final class JsonLdRdfObject implements RdfObject {

    private final RdfLiteral literal;
    private final String string;
    
    protected JsonLdRdfObject(RdfLiteral literal) {
        this.literal = literal;
        this.string = null;
    }

    protected JsonLdRdfObject(String string) {
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
