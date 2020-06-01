package com.apicatalog.mini.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.mini.rdf.MiniRdfDataset;
import com.apicatalog.mini.rdf.io.MiniRdfReader;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class MiniRdfProvider extends RdfProvider {

    public static final RdfProvider DEFAULT = new MiniRdfProvider(); 
    
    @Override
    public Dataset createDataset() {
        return new MiniRdfDataset();
    }

    @Override
    public RdfReader createReader(Reader reader) {
        return new MiniRdfReader(reader);
    }

    @Override
    public RdfWriter createWriter(Writer writer) {
        // TODO Auto-generated method stub
        return null;
    }

}
