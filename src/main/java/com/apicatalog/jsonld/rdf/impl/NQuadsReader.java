package com.apicatalog.jsonld.rdf.impl;

import java.io.Reader;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.io.RdfReader;

final class NQuadsReader implements RdfReader {

    private final Reader reader;
    
    protected NQuadsReader(final Reader reader) {
        this.reader = reader;
    }
    
    @Override
    public RdfDataset readDataset() {
        //FIXME
        return new RdfDatasetImpl();
    }

}
