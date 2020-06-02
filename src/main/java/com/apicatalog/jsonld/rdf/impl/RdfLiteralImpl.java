package com.apicatalog.jsonld.rdf.impl;

import java.util.Optional;

import com.apicatalog.jsonld.rdf.RdfLiteral;

final class RdfLiteralImpl implements RdfLiteral {

    private final String value;
    
    protected RdfLiteralImpl(String value) {
        this.value = value;
    }
    
    @Override
    public String getValue() {
        return value;
    }

    @Override
    public String getDatatype() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Optional<String> getLanguage() {
        // TODO Auto-generated method stub
        return null;
    }
    
}
