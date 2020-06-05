package com.apicatalog.rdf;

import java.util.List;
import java.util.stream.Stream;

public interface RdfDataset {

    RdfGraph getDefaultGraph();

    void add(RdfNQuad nquad);
        
    Stream<RdfNQuad> stream();
    
    List<RdfNQuad> toList();
    
    Stream<RdfGraphName> getGraphNames();
    
    RdfGraph getGraph(RdfGraphName graphName);

    Stream<NamedGraph> getNamedGraphs();
    
    /**
     * 
     * @return total number of n-quads in the dataset 
     */
    
    int size();

    static class NamedGraph {

        RdfGraph graph;
        RdfGraphName graphName;
        
        public NamedGraph(RdfGraphName graphName, RdfGraph graph) {
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
