package com.apicatalog.jsonld.iri;

import java.util.Objects;

import com.apicatalog.jsonld.uri.UriUtils;

public final class IRI {

    private final String stringValue;
    
    private IRI(String value) {
        this.stringValue = value;
    }
    
    public static final IRI create(String iri) {
        if (iri == null) {
            throw new IllegalArgumentException();
        }

        
        return new IRI(iri);
    }

    @Override
    public String toString() {
        return stringValue;
    }

    /**
     * 
     * @see <a href="https://tools.ietf.org/html/rfc3987#section-2.2">ABNF for IRI References and IRIs</a>
     */
    public static boolean isWellFormed(String iri) {
        //TODO        
        return UriUtils.isURI(iri);
    }

    @Override
    public int hashCode() {
        return Objects.hash(stringValue);
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
        IRI other = (IRI) obj;
        return Objects.equals(stringValue, other.stringValue);
    }
}
