package com.apicatalog.jsonld.rdf.io.impl;

import java.io.Reader;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.io.RdfReader;

public final class RdfNQuadsReader implements RdfReader {

    private final Reader reader;
    
    public RdfNQuadsReader(final Reader reader) {
        this.reader = reader;
    }
    
    @Override
    public RdfDataset getDataset() {
        // TODO Auto-generated method stub
        return null;
    }

}
