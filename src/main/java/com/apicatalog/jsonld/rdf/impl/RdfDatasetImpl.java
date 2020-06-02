package com.apicatalog.jsonld.rdf.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfGraph;
import com.apicatalog.jsonld.rdf.RdfNQuad;

final class RdfDatasetImpl implements RdfDataset {

    private final Map<String, RdfGraph> graphs;
    private RdfGraph defaultGraph;
    
    protected RdfDatasetImpl() {
        this.graphs = new HashMap<>();
        this.defaultGraph = new RdfGraphImpl();
    }
    
    @Override
    public RdfGraph getDefaultGraph() {
        return defaultGraph;
    }

    @Override
    public void add(String graphName, RdfGraph graph) {
        graphs.put(graphName, graph);
    }

    @Override
    public Stream<NamedGraph> stream() {
        // TODO Auto-generated method stub
        return null;
    }

    public void add(RdfNQuad nquad) {

        RdfGraph graph = defaultGraph;
        
        if (nquad.getGraphName() != null) {
            
            graph = graphs.get(nquad.getGraphName());
            
            if (graph == null) {
                graph = new RdfGraphImpl();
                graphs.put(nquad.getGraphName(), graph);
            }
        }
        
        graph.add(nquad);
    }
    
}
