package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;

final class RdfDatasetImpl implements RdfDataset {

    private final Map<RdfGraphName, RdfGraphImpl> graphs;
    private final List<RdfNQuad> nquads;
    
    private RdfGraphImpl defaultGraph;
    
    protected RdfDatasetImpl() {
        this.graphs = new HashMap<>();
        this.nquads = new LinkedList<>();
        this.defaultGraph = new RdfGraphImpl();
    }
    
    @Override
    public RdfGraph getDefaultGraph() {
        return defaultGraph;
    }

//    @Override
//    public void add(RdfGraphName graphName, RdfGraph graph) {
//        graphs.put(graphName, graph);
//    }
        
    @Override
    public Stream<RdfNQuad> stream() {
        return nquads.stream();
    }
    
    @Override
    public List<? extends RdfNQuad> toList() {
        return nquads;
    }

    public void add(RdfNQuad nquad) {

        RdfGraphImpl graph = defaultGraph;
        
        if (nquad.getGraphName() != null) {
            
            graph = graphs.get(nquad.getGraphName());
            
            if (graph == null) {
                graph = new RdfGraphImpl();
                graphs.put(nquad.getGraphName(), graph);
            }
        }
        nquads.add(nquad);
        graph.add(nquad);
    }
    
    @Override
    public Stream<RdfGraphName> getGraphNames() {
        return graphs.keySet().stream();
    }

    @Override
    public RdfGraph getGraph(RdfGraphName graphName) {
        return graphs.get(graphName);
    }

    @Override
    public int size() {
        return graphs.values().stream().map(RdfGraph::size).reduce(getDefaultGraph().size(), Integer::sum);           
    }
}
