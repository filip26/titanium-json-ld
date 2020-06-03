package com.apicatalog.jsonld.rdf.impl;

import java.util.Objects;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.rdf.RdfNQuad;
import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfSubject;

final class RdfNQuadImpl implements RdfNQuad {

    private final RdfSubject subject;
    private final IRI predicate;
    private final RdfObject object;
    private final String graphName;
    
    protected RdfNQuadImpl(RdfSubject subject, IRI predicate, RdfObject object, String graphName) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
        this.graphName = graphName;
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
    public String getGraphName() {
        return graphName;
    }

    @Override
    public int hashCode() {
        return Objects.hash(graphName, object, predicate, subject);
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

        RdfNQuadImpl other = (RdfNQuadImpl) obj;
        return Objects.equals(graphName, other.graphName) && Objects.equals(object, other.object)
                && Objects.equals(predicate, other.predicate) && Objects.equals(subject, other.subject);
    }
}
