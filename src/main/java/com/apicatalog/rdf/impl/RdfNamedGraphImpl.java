package com.apicatalog.rdf.impl;

import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNamedGraph;

class RdfNamedGraphImpl implements RdfNamedGraph {

    RdfGraph graph;
    RdfGraphName graphName;
    
    protected RdfNamedGraphImpl(RdfGraphName graphName, RdfGraph graph) {
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
