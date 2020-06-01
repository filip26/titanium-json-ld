package com.apicatalog.jsonld.rdf.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import com.apicatalog.jsonld.rdf.RdfDataset;
import com.apicatalog.jsonld.rdf.RdfGraph;

final class JsonLdRdfDataset implements RdfDataset {

    private final Map<String, RdfGraph> graphs;
    private RdfGraph defaultGraph;
    
    protected JsonLdRdfDataset() {
        this.graphs = new HashMap<>();
        this.defaultGraph = new JsonLdRdfGraph();
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

//    protected MiniRdfDataset() {
//        // TODO Auto-generated constructor stub
//    }
//    
//    @Override
//    public void add(Quad quad) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public void add(BlankNodeOrIRI graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public boolean contains(Quad quad) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public boolean contains(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public Graph getGraph() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public Optional<Graph> getGraph(BlankNodeOrIRI graphName) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public Stream<BlankNodeOrIRI> getGraphNames() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public void remove(Quad quad) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public void remove(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public void clear() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public long size() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public Stream<? extends Quad> stream() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public Stream<? extends Quad> stream(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
//        throw new UnsupportedOperationException();
//    }

}
