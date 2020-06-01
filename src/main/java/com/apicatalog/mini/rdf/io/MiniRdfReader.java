package com.apicatalog.mini.rdf.io;

import java.io.Reader;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.rdf.io.RdfReader;

public final class MiniRdfReader implements RdfReader {

    private final Reader reader;
    
    public MiniRdfReader(final Reader reader) {
        this.reader = reader;
    }
    
    @Override
    public Dataset getDataset() {
        // TODO Auto-generated method stub
        return null;
    }

}
