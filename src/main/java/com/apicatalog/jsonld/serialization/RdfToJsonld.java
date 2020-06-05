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
    private boolean useRdfFlag;
    
    private RdfToJsonld(final RdfDataset dataset) {
        this.dataset = dataset;
    }
    
    public static final RdfToJsonld with(final RdfDataset dataset) {
        return new RdfToJsonld(dataset);
    }
    
    
    public JsonArray build() throws JsonLdError {
        return JsonValue.EMPTY_JSON_ARRAY;
    }
}
