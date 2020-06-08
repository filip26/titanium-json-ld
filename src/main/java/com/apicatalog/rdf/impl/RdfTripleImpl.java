package com.apicatalog.rdf.impl;

import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;

class RdfTripleImpl implements RdfTriple {

    private final RdfSubject subject;
    
    private final RdfPredicate predicate;
    
    private final RdfObject object;
    
    protected RdfTripleImpl(final RdfSubject subject, final RdfPredicate predicate, final RdfObject object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }

    @Override
    public RdfSubject getSubject() {
        return subject;
    }

    @Override
    public RdfPredicate getPredicate() {
        return predicate;
    }

    @Override
    public RdfObject getObject() {
        return object;
    }
}
