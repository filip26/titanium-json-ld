package com.apicatalog.jsonld.rdf.impl;

import java.io.Reader;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.io.RdfReader;

final class JsonLdNQuadsReader implements RdfReader {

    private final Reader reader;
    
    protected JsonLdNQuadsReader(final Reader reader) {
        this.reader = reader;
    }
    
    @Override
    public RdfDataset getDataset() {
        // TODO Auto-generated method stub
        return null;
    }

}
