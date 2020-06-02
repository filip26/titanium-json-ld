package com.apicatalog.jsonld.rdf.io;

import java.io.Reader;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfTriple;

public interface RdfReader {
    
    RdfDataset readDataset();
    
}
