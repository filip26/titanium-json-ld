package com.apicatalog.jsonld.rdf.impl;

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
}
