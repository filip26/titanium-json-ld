package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfNamedGraph;

final class RdfDatasetImpl implements RdfDataset {

    private final Map<RdfGraphName, RdfGraph> graphs;
    
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
    public void add(RdfGraphName graphName, RdfGraph graph) {
        graphs.put(graphName, graph);
    }
    
    @Override
    public void add(RdfNamedGraph namedGraph) {
        graphs.put(namedGraph.getGraphName(), namedGraph.getGraph());
    }

    
    @Override
    public Stream<RdfNamedGraph> stream() {
        return graphs.entrySet().stream().map(e -> new RdfNamedGraphImpl(e.getKey(), e.getValue()));
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

    @Override
    public int size() {
        return graphs.values().stream().map(RdfGraph::size).reduce(getDefaultGraph().size(), Integer::sum);           
    }
    
}
