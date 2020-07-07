package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfResource;

final class RdfResourceImpl implements RdfResource {

    private final String value;
    private final boolean blankNode;
    
    protected RdfResourceImpl(final String value, boolean isBlankNode) {
        this.value = value;
        this.blankNode = isBlankNode;
    }

    @Override
    public boolean isBlankNode() {
        return blankNode;
    }
    
    @Override
    public boolean isIRI() {
        return !blankNode;
    }
    
    @Override
    public String getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
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
        RdfResourceImpl other = (RdfResourceImpl) obj;
        return Objects.equals(value, other.value);
    }
    
    @Override
    public String toString() {
        return Objects.toString(value);
    }
}
