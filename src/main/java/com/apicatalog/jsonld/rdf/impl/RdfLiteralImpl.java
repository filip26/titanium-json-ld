package com.apicatalog.jsonld.rdf.impl;

import java.util.Objects;
import java.util.Optional;

import com.apicatalog.jsonld.iri.IRI;
import com.apicatalog.jsonld.rdf.RdfLiteral;

final class RdfLiteralImpl implements RdfLiteral {

    private final String value;
    private final IRI dataType;
    private final String langTag;
    
    protected RdfLiteralImpl(String value) {
        this.value = value;
        this.dataType = null;
        this.langTag = null; 
    }

    protected RdfLiteralImpl(String value, IRI dataType) {
        this.value = value;
        this.dataType = dataType;
        this.langTag = null;
    }

    protected RdfLiteralImpl(String value, String langTag) {
        this.value = value;
        this.langTag = langTag;
        this.dataType = IRI.create("rdf:langString");
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public IRI getDatatype() {
        return dataType;
    }

    @Override
    public Optional<String> getLanguage() {
        return Optional.ofNullable(langTag);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dataType, langTag, value);
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
        RdfLiteralImpl other = (RdfLiteralImpl) obj;
        return Objects.equals(dataType, other.dataType) && Objects.equals(langTag, other.langTag)
                && Objects.equals(value, other.value);
    }
    
}
