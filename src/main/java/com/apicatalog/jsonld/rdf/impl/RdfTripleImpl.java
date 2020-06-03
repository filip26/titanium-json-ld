package com.apicatalog.jsonld.rdf.impl;

import java.util.Objects;

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

    public static RdfTriple create(RdfSubject subject, IRI predicate, RdfObject object) {
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

    @Override
    public int hashCode() {
        return Objects.hash(object, predicate, subject);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RdfTripleImpl other = (RdfTripleImpl) obj;
        return Objects.equals(object, other.object) && Objects.equals(predicate, other.predicate)
                && Objects.equals(subject, other.subject);
    }
}
