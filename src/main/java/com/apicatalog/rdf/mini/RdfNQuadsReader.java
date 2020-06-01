package com.apicatalog.rdf.mini;

import java.io.Reader;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.rdf.io.RdfReader;

final class RdfNQuadsReader implements RdfReader {

    private final Reader reader;
    
    protected RdfNQuadsReader(final Reader reader) {
        this.reader = reader;
    }
    
    @Override
    public Dataset getDataset() {
        // TODO Auto-generated method stub
        return null;
    }

}
