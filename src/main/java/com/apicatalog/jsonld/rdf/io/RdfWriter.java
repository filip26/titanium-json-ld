package com.apicatalog.jsonld.rdf.io;

import java.io.IOException;

import com.apicatalog.jsonld.rdf.RdfDataset;

public interface RdfWriter {

    void write(RdfDataset dataset) throws IOException;
    
}
