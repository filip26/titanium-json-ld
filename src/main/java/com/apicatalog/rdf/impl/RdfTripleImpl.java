package com.apicatalog.rdf.impl;

import com.apicatalog.iri.IRI;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;

final class RdfTripleImpl implements RdfTriple {

    private final RdfSubject subject;
    private final IRI predicate;
    private final RdfObject object;
    
    protected RdfTripleImpl(RdfSubject subject, IRI predicate, RdfObject object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }

    public static RdfTriple create(RdfSubject subject, IRI predicate, RdfObject object) {
        return new RdfTripleImpl(subject, predicate, object);
    }

    public static RdfTriple create(IRI subject, IRI predicate, RdfObject object) {
        return new RdfTripleImpl(new RdfSubjectImpl(subject), predicate, object);
    }
    
    public static RdfTriple create(IRI subject, IRI predicate, IRI object) {
        return create(new RdfSubjectImpl(subject), predicate, RdfObjectImpl.of(object));
    }

    @Override
    public RdfSubject getSubject() {
        return subject;
    }

    @Override
    public IRI getPredicate() {
        return predicate;
    }

    @Override
    public RdfObject getObject() {
        return object;
    }
}
