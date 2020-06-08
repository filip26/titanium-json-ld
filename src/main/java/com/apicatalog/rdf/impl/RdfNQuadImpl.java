package com.apicatalog.rdf.impl;

import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;

final class RdfNQuadImpl extends RdfTripleImpl implements RdfNQuad {

    private final RdfGraphName graphName;
    
    protected RdfNQuadImpl(RdfSubject subject, RdfPredicate predicate, RdfObject object, RdfGraphName graphName) {
        super(subject, predicate, object);
        this.graphName = graphName;
    }
    
    @Override
    public RdfGraphName getGraphName() {
        return graphName;
    }
}
