package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfObject;

final class RdfObjectImpl implements RdfObject {

    private final RdfLiteral literal;
    
    private final String value;
    
    private final Type type;
    
    protected RdfObjectImpl(final RdfLiteral literal) {
        this.literal = literal;
        this.value = null;
        this.type = Type.LITERAL;
    }

    protected RdfObjectImpl(final Type type, final String value) {
        this.literal = Type.LITERAL.equals(type) ? new RdfLiteralImpl(value) : null;
        this.value = Type.LITERAL.equals(type) ? null : value;
        this.type = type;
    }
    
    @Override
    public boolean isLiteral() {
        return Type.LITERAL.equals(type);
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
    public RdfLiteral getLiteral() {
        return literal;
    }

    @Override
    public String toString() {
        if (literal != null) {
            return literal.toString();
        }
        return value == null ? "null" : value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, type, literal);
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
        RdfObjectImpl other = (RdfObjectImpl) obj;
        return Objects.equals(value, other.value) && type == other.type
                && Objects.equals(literal, other.literal);
    }    
}
