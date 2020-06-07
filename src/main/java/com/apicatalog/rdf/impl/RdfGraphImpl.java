package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.iri.IRI;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;

final class RdfGraphImpl implements RdfGraph {

    private final Map<RdfSubject, Map<IRI, Set<RdfObject>>> index;
    
    private final List<RdfTriple> triples;
    
    protected RdfGraphImpl() {
        this.index = new HashMap<>();
        this.triples = new LinkedList<>();
    }

    public void add(RdfTriple triple) {
        index
            .computeIfAbsent(triple.getSubject(), x -> new HashMap<IRI, Set<RdfObject>>())
            .computeIfAbsent(triple.getPredicate(), x -> new HashSet<RdfObject>())
            .add(triple.getObject());
        
        triples.add(triple);
    }
    
    @Override
    public boolean contains(RdfTriple triple) {
        return index.containsKey(triple.getSubject()) 
                    && index.get(triple.getSubject()).containsKey(triple.getPredicate())
                    && index.get(triple.getSubject()).get(triple.getPredicate()).contains(triple.getObject())
                    ;
    }

    @Override
    public List<RdfTriple> toList() {
        return triples;
    }
}
