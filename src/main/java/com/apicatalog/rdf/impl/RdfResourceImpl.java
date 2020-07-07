package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;

final class RdfResourceImpl implements RdfResource {

    private final RdfValue.ValueType type;
    private final String value;
    
    protected RdfResourceImpl(final RdfValue.ValueType type, final String value) {
        this.type = type;
        this.value = value;
    }
    
    @Override
    public boolean isLiteral() {
        return false;
    }

    @Override
    public boolean isIRI() {
        return RdfValue.ValueType.IRI.equals(type);
    }

    @Override
    public boolean isBlankNode() {
        return RdfValue.ValueType.BLANK_NODE.equals(type);
    }

    @Override
    public ValueType getValueType() {
        return type;
    }

    @Override
    public RdfResource asResource() {
        return this;
    }

    @Override
    public RdfLiteral asLiteral() {
        throw new ClassCastException();
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
