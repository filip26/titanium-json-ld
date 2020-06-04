package com.apicatalog.rdf;

import java.util.List;
import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

//    void add(RdfGraphName graphName, RdfGraph graph);

    void add(RdfNQuad nquad);
        
    Stream<? extends RdfNQuad> stream();
    
    List<? extends RdfNQuad> toList();
    
    Stream<RdfGraphName> getGraphNames();
    
    RdfGraph getGraph(RdfGraphName graphName);
    
    /**
     * 
     * @return total number of n-quads in the dataset 
     */
    
    int size();

    static class NamedGraph {

        RdfGraph graph;
        RdfGraphName graphName;
        
        protected NamedGraph(RdfGraphName graphName, RdfGraph graph) {
            this.graphName = graphName;
            this.graph = graph;
        }
        
        public RdfGraph getGraph() {
            return graph;
        }
        
        public RdfGraphName getGraphName() {
            return graphName;
        }

    }
}
