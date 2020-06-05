package com.apicatalog.rdf.io;

import java.io.IOException;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.nquad.NQuadsWriterError;

public interface RdfWriter {

    void write(RdfDataset dataset) throws IOException, NQuadsWriterError;
    
}
