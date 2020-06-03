package com.apicatalog.rdf.io;

import java.io.IOException;

import com.apicatalog.rdf.RdfDataset;

public interface RdfWriter {

    void write(RdfDataset dataset) throws IOException;
    
}
