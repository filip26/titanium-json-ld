package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfSubject;
import com.apicatalog.jsonld.rdf.RdfTriple;

final class RdfTripleImpl implements RdfTriple {

    private final RdfSubject subject;
    private final IRI predicate;
    private final RdfObject object;
    
    protected RdfTripleImpl(RdfSubject subject, IRI predicate, RdfObject object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }

    public static RdfTriple create(RdfSubjectImpl subject, IRI predicate, RdfObject object) {
        return new RdfTripleImpl(subject, predicate, object);   //FIXME
    }

    public static RdfTriple create(IRI subject, IRI predicate, RdfObject object) {
        return new RdfTripleImpl(new RdfSubjectImpl(subject), predicate, object);   //FIXME
    }

    
    public static RdfTriple create(IRI subject, IRI predicate, IRI object) {
        return create(new RdfSubjectImpl(subject), predicate, RdfObjectImpl.of(object));   //FIXME
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
