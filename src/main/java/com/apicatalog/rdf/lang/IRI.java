package com.apicatalog.rdf.lang;

import com.apicatalog.jsonld.uri.UriUtils;

public final class IRI {

    private IRI() {
    }
    
    /**
     * 
     * @see <a href="https://tools.ietf.org/html/rfc3987#section-2.2">ABNF for IRI References and IRIs</a>
     */
    public static boolean isWellFormed(String iri) {
        
        if (iri == null) {
            throw new IllegalArgumentException();
        }

        //TODO        
        return UriUtils.isAbsoluteUri(iri);
    }
}
