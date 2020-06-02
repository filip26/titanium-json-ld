package com.apicatalog.jsonld.rdf.impl;

import com.apicatalog.jsonld.rdf.RdfObject;
import com.apicatalog.jsonld.rdf.RdfTriple;

final class JsonLdRdfTriple implements RdfTriple {

    private final String subject;
    private final String predicate;
    private final RdfObject object;
    
    protected JsonLdRdfTriple(String subject, String predicate, RdfObject object) {
        this.subject = subject;
        this.predicate = predicate; 
        this.object = object;
    }
    
    protected static final RdfTriple of(String subject, String predicate, String object) {
        return new JsonLdRdfTriple(subject, predicate, new JsonLdRdfObject(object));
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

}
