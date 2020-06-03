package com.apicatalog.jsonld.iri;

import com.apicatalog.jsonld.uri.UriUtils;

public final class IRI {

    private final String value;
    
    private IRI(String value) {
        this.value = value;
    }
    
    public static final IRI create(String iri) {
        if (iri == null) {
            throw new IllegalArgumentException();
        }

        
        return new IRI(iri);
    }

    @Override
    public String toString() {
        return value;
    }


    /**
     * 
     * @see <a href="https://tools.ietf.org/html/rfc3987#section-2.2">ABNF for IRI References and IRIs</a>
     */
    public static boolean isWellFormed(String iri) {
        //TODO        
        return UriUtils.isURI(iri);
    }
}
