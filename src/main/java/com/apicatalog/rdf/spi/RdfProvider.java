package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfFormat;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;

public abstract class RdfProvider {

    protected RdfProvider() {
        
    }
    
    public static RdfProvider provider() {
        //TODO
        return DefaultRdfProvider.DEFAULT;
    }

    public abstract RdfDataset createDataset();

    public abstract RdfReader createReader(Reader reader, RdfFormat format);

    public abstract RdfWriter createWriter(Writer writer, RdfFormat format);

    public abstract RdfGraph createGraph();
    
}
