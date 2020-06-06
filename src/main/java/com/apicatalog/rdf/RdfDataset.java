package com.apicatalog.rdf;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(RdfNQuad nquad);
        
    Stream<RdfNQuad> stream();
    
    List<RdfNQuad> toList();
    
    Set<RdfGraphName> getGraphNames();
    
    RdfGraph getGraph(RdfGraphName graphName);

    /**
     * 
     * @return total number of n-quads in the dataset 
     */    
    int size();

}
