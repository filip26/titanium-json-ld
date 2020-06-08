package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfPredicate;

final class RdfPredicateImpl implements RdfPredicate {

    private final Type type;
    
    private final String value;
    
    protected RdfPredicateImpl(final Type type, final String value) {
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
        return Objects.hash(type, value);
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
        RdfPredicateImpl other = (RdfPredicateImpl) obj;
        return type == other.type && Objects.equals(value, other.value);
    }

}
