package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
        
    @Override
    public Stream<RdfNQuad> stream() {
        return nquads.stream();
    }
    
    @Override
    public List<RdfNQuad> toList() {
        return nquads;
    }
    
    public void add(RdfNQuad nquad) {

        if (nquad.getGraphName() != null) {

            RdfGraphImpl graph = graphs.get(nquad.getGraphName());
            
            if (graph == null) {
                graph = new RdfGraphImpl();
                graphs.put(nquad.getGraphName(), graph);
                graph.add(nquad);
                nquads.add(nquad);
                
            } else if (!graph.contains(nquad)) {
                graph.add(nquad);
                nquads.add(nquad);
            }
    
        } else {
            // add to default graph
            if (!defaultGraph.contains(nquad)) {
                defaultGraph.add(nquad);
                nquads.add(nquad);
            }
        }
    }
    
    @Override
    public Set<RdfGraphName> getGraphNames() {
        return graphs.keySet();
    }

    @Override
    public RdfGraph getGraph(RdfGraphName graphName) {
        return graphs.get(graphName);
    }

    @Override
    public int size() {
        return nquads.size();           
    }
}
