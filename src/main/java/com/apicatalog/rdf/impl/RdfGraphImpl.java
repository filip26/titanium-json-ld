package com.apicatalog.rdf.impl;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Stream;

import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfTriple;

final class RdfGraphImpl implements RdfGraph {

    private final List<RdfTriple> triples;
    
    protected RdfGraphImpl() {
        this.triples = new LinkedList<>();
    }

    public void add(RdfTriple triple) {
        triples.add(triple);
    }
    
    @Override
    public Stream<? extends RdfTriple> stream() {
        return triples.stream();
    }

    @Override
    public List<? extends RdfTriple> toList() {
        return triples;
    }

    @Override
    public int size() {
        return triples.size();
    }

}
