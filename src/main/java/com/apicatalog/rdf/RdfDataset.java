package com.apicatalog.rdf;

import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(RdfGraphName graphName, RdfGraph graph);

    void add(RdfNQuad nquad);
    
    void add(RdfNamedGraph graph);
    
    Stream<RdfNamedGraph> stream();

    /**
     * 
     * @return total number of n-quads in the dataset 
     */
    
    int size();

}
