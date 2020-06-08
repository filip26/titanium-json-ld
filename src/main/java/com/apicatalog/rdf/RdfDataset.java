package com.apicatalog.rdf;

import java.util.List;
import java.util.Set;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(RdfNQuad nquad);
        
    List<RdfNQuad> toList();
    
    Set<RdfGraphName> getGraphNames();
    
    RdfGraph getGraph(RdfGraphName graphName);

    /**
     * 
     * @return total number of n-quads in the dataset 
     */    
    int size();

}
