package com.apicatalog.rdf.mini;

import java.util.Optional;
import java.util.stream.Stream;

import org.apache.commons.rdf.api.BlankNodeOrIRI;
import org.apache.commons.rdf.api.Dataset;
import org.apache.commons.rdf.api.Graph;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.api.Quad;
import org.apache.commons.rdf.api.RDFTerm;

class MiniRdfDataset implements Dataset {

    protected MiniRdfDataset() {
        // TODO Auto-generated constructor stub
    }
    
    @Override
    public void add(Quad quad) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void add(BlankNodeOrIRI graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean contains(Quad quad) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean contains(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Graph getGraph() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Graph> getGraph(BlankNodeOrIRI graphName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Stream<BlankNodeOrIRI> getGraphNames() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void remove(Quad quad) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void remove(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public long size() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Stream<? extends Quad> stream() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Stream<? extends Quad> stream(Optional<BlankNodeOrIRI> graphName, BlankNodeOrIRI subject, IRI predicate, RDFTerm object) {
        throw new UnsupportedOperationException();
    }

}
