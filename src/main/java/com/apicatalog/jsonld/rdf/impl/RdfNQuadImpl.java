package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.rdf.RdfNQuad;
import com.apicatalog.jsonld.rdf.RdfObject;

final class RdfNQuadImpl implements RdfNQuad {

    private final String subject;
    private final String predicate;
    private final RdfObject object;
    private final String graphName;
    
    protected RdfNQuadImpl(String subject, String predicate, RdfObject object, String graphName) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
        this.graphName = graphName;
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

    @Override
    public String getGraphName() {
        return graphName;
    }
}
