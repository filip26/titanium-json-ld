package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfSubject;

class RdfSubjectImpl implements RdfSubject {

    private final String value;
    
    private final Type type;
    
    protected RdfSubjectImpl(final Type type, final String value) {
        this.value = value;
        this.type = type;
    }

    @Override
    public boolean isIRI() {
        return Type.IRI.equals(type);
    }
    
    @Override
    public boolean isBlankNode() {
        return Type.BLANK_NODE.equals(type);
    }
    
    @Override
    public String toString() {
        return value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, type);
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
        RdfSubjectImpl other = (RdfSubjectImpl) obj;
        return Objects.equals(value, other.value) && type == other.type;
    }
}
