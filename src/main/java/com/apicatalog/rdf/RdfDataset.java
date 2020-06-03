package com.apicatalog.rdf;

import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(String graphName, RdfGraph graph);

    void add(RdfNQuad nquad);
    
    Stream<NamedGraph> stream();

    class NamedGraph {

        RdfGraph graph;
        String graphName;
        
        public NamedGraph(String graphName, RdfGraph graph) {
            this.graphName = graphName;
            this.graph = graph;
        }
        
        public RdfGraph getGraph() {
            return graph;
        }
        
        
    }

    /**
     * 
     * @return total number of n-quads in the dataset 
     */
    
    int size();

}
