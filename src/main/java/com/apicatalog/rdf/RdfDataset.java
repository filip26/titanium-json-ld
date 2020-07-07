package com.apicatalog.rdf;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(RdfNQuad nquad);
        
    List<RdfNQuad> toList();
    
    Set<RdfResource> getGraphNames();
    
    Optional<RdfGraph> getGraph(RdfResource graphName);

    /**
     * 
     * @return total number of N-Quads in the dataset 
     */    
    int size();
}
