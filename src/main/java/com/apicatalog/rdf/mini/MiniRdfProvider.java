package com.apicatalog.rdf.mini;

import java.io.Reader;
import java.io.Writer;

import org.apache.commons.rdf.api.Dataset;

import com.apicatalog.rdf.RdfFormat;
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

}
