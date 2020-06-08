package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfGraphName;

class RdfGraphNameImpl implements RdfGraphName {

    private final String value;
    private final Type type;
    
    protected RdfGraphNameImpl(Type type, String value) {
        this.type = type;
        this.value = value;
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
        RdfGraphNameImpl other = (RdfGraphNameImpl) obj;
        return Objects.equals(value, other.value) && type == other.type;
    }
    
}
