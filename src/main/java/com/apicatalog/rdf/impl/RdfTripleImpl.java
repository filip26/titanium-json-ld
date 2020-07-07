package com.apicatalog.rdf.impl;

import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;

class RdfTripleImpl implements RdfTriple {

    private final RdfResource subject;
    
    private final RdfResource predicate;
    
    private final RdfValue object;
    
    protected RdfTripleImpl(final RdfResource subject, final RdfResource predicate, final RdfValue object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }

    @Override
    public RdfResource getSubject() {
        return subject;
    }

    @Override
    public RdfResource getPredicate() {
        return predicate;
    }

    @Override
    public RdfValue getObject() {
        return object;
    }
}
