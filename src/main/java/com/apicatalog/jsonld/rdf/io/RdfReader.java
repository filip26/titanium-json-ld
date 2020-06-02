package com.apicatalog.jsonld.rdf.io;

import java.io.IOException;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.nq.impl.NQuadsReaderError;

public interface RdfReader {
    
    RdfDataset readDataset() throws IOException, NQuadsReaderError;
    
}
