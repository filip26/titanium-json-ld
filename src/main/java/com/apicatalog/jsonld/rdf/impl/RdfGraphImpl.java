package com.apicatalog.jsonld.rdf.impl;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Stream;

import com.apicatalog.jsonld.rdf.RdfGraph;
import com.apicatalog.jsonld.rdf.RdfTriple;

final class RdfGraphImpl implements RdfGraph {

    private final List<RdfTriple> triples;
    
    protected RdfGraphImpl() {
        this.triples = new LinkedList<>();
    }
    
    @Override
    public void add(final RdfTriple triple) {
        triples.add(triple);   
    }

    @Override
    public Stream<? extends RdfTriple> stream() {
        return triples.stream();
    }

    @Override
    public List<? extends RdfTriple> getList() {
        return triples;
    }

    @Override
    public int size() {
        return triples.size();
    }

}
