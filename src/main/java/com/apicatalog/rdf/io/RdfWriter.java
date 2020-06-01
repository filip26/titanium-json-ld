package com.apicatalog.rdf.io;

import org.apache.commons.rdf.api.Dataset;

public interface RdfWriter {

    void writeDataset(Dataset dataset);
    
}
