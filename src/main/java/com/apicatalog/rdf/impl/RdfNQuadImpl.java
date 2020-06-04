package com.apicatalog.rdf.impl;

import com.apicatalog.iri.IRI;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;

final class RdfNQuadImpl implements RdfNQuad {

    private final RdfSubject subject;
    private final IRI predicate;
    private final RdfObject object;
    private final RdfGraphName graphName;
    
    protected RdfNQuadImpl(RdfSubject subject, IRI predicate, RdfObject object, RdfGraphName graphName) {
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
    public RdfGraphName getGraphName() {
        return graphName;
    }
}
