package com.apicatalog.jsonld.rdf.impl;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfFormat;
import com.apicatalog.jsonld.rdf.RdfGraph;
import com.apicatalog.jsonld.rdf.io.RdfReader;
import com.apicatalog.jsonld.rdf.io.RdfWriter;
import com.apicatalog.jsonld.rdf.spi.RdfProvider;

public final class JsonLdRdfProvider extends RdfProvider {

    public static final RdfProvider INSTANCE = new JsonLdRdfProvider(); 
    
    @Override
    public RdfDataset createDataset() {
        return new JsonLdRdfDataset();
    }

    @Override
    public RdfReader createReader(Reader reader, RdfFormat format) {
        
        if (RdfFormat.NQuads.equals(format)) {
            return new JsonLdNQuadsReader(reader);            
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
        return new JsonLdRdfGraph();
    }

}
