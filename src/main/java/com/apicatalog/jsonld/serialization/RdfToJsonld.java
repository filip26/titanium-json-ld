package com.apicatalog.jsonld.serialization;

import javax.json.JsonArray;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.rdf.RdfDataset;

public final class RdfToJsonld {

    // required
    private RdfDataset dataset;
    
    // optional
    private boolean ordered;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    private boolean useRdfType;
    
    private RdfToJsonld(final RdfDataset dataset) {
        this.dataset = dataset;
        
        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
    }
    
    public static final RdfToJsonld with(final RdfDataset dataset) {
        return new RdfToJsonld(dataset);
    }
    
    public RdfToJsonld ordered() {
        return ordered(true);
    }
    
    public RdfToJsonld ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public RdfToJsonld rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfToJsonld useNativeTypes() {
        return useNativeTypes(false);
    }
    
    public RdfToJsonld useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }
    
    public RdfToJsonld useRdfType() {
        return useRdfType(true);
    }
    
    public RdfToJsonld useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }
    
    public JsonArray build() throws JsonLdError {
        
        // 1.
        
        return JsonValue.EMPTY_JSON_ARRAY;
    }
}
