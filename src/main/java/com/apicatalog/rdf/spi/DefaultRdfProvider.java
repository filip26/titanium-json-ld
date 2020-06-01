package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfFormat;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.impl.RdfDatasetImpl;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.impl.RdfNQuadsReader;

public final class DefaultRdfProvider extends RdfProvider {

    public static final RdfProvider DEFAULT = new DefaultRdfProvider(); 
    
    @Override
    public RdfDataset createDataset() {
        return new RdfDatasetImpl();
    }

    @Override
    public RdfReader createReader(Reader reader, RdfFormat format) {
        
        if (RdfFormat.NQuads.equals(format)) {
            return new RdfNQuadsReader(reader);            
        }
        //TODO
        return null;
    }

    @Override
    public RdfWriter createWriter(Writer writer, RdfFormat format) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public RdfGraph createGraph() {
        // TODO Auto-generated method stub
        return null;
    }

}
