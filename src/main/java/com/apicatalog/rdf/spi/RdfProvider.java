package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.rdf.io.RdfParser;
import com.apicatalog.rdf.io.RdfWriter;

public abstract class RdfProvider {

    protected RdfProvider() {
        
    }
    
    public static RdfProvider provider() {
        //TODO
        return null;
    }

    public abstract Dataset createDataset();

    public abstract RdfParser createParser(Reader reader);

    public abstract RdfWriter createWriter(Writer writer);
    
}
