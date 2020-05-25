package com.apicatalog.jsonld.compaction;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#iri-compaction">IRI Compaction</a>
 *
 */
public final class UriCompactionBuilder {

    private final ActiveContext activeContext;
    private final String value;
    
    public UriCompactionBuilder(final ActiveContext activeContext, final String value) {
        this.activeContext = activeContext;
        this.value = value;
    }
    
    public static UriCompactionBuilder with(ActiveContext activeContext, String value) {
        return new UriCompactionBuilder(activeContext, value);
    }
    
    public String build() throws JsonLdError {
        //TODO
        
        return value;
    }

}
