package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.mini.rdf.spi.MiniRdfProvider;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;

public abstract class RdfProvider {

    protected RdfProvider() {
        
    }
    
    public static RdfProvider provider() {
        //TODO
        return MiniRdfProvider.DEFAULT;
    }

    public abstract Dataset createDataset();

    public abstract RdfReader createReader(Reader reader);

    public abstract RdfWriter createWriter(Writer writer);
    
}
