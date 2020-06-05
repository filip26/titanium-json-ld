package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.apicatalog.iri.IRI;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;

final class RdfGraphImpl implements RdfGraph {

    private final Map<RdfSubject, Map<IRI, Set<RdfObject>>> triples;
    
    protected RdfGraphImpl() {
        this.triples = new HashMap<>();
    }

    public void add(RdfTriple triple) {
        triples
            .computeIfAbsent(triple.getSubject(), x -> new HashMap<IRI, Set<RdfObject>>())
            .computeIfAbsent(triple.getPredicate(), x -> new HashSet<RdfObject>())
            .add(triple.getObject());        
    }
    
    @Override
    public boolean contains(RdfTriple triple) {
        return triples.containsKey(triple.getSubject()) 
                    && triples.get(triple.getSubject()).containsKey(triple.getPredicate())
                    && triples.get(triple.getSubject()).get(triple.getPredicate()).contains(triple.getObject())
                    ;
    }

}
