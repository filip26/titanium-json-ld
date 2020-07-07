package com.apicatalog.rdf.impl;

import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;

final class RdfNQuadImpl extends RdfTripleImpl implements RdfNQuad {

    private final RdfResource graphName;
    
    protected RdfNQuadImpl(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName) {
        super(subject, predicate, object);
        this.graphName = graphName;
    }
    
    @Override
    public RdfResource getGraphName() {
        return graphName;
    }
}
