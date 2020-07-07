package com.apicatalog.rdf.io;

import java.io.IOException;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.error.RdfReaderException;

public interface RdfReader {
    
    RdfDataset readDataset() throws IOException, RdfReaderException;
    
}