package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfTriple;

final class RdfTripleImpl implements RdfTriple {

    private final String subject;
    private final String predicate;
    private final RdfObject object;
    
    protected RdfTripleImpl(String subject, String predicate, RdfObject object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }
    
    protected static final RdfTriple of(String subject, String predicate, String object) {
        return new RdfTripleImpl(subject, predicate, new RdfObjectImpl(object));
    }
    
    @Override
    public String getSubject() {
        return subject;
    }

    @Override
    public String getPredicate() {
        return predicate;
    }

    @Override
    public RdfObject getObject() {
        return object;
    }
}
