package com.apicatalog.jsonld.processor;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.serialization.RdfToJsonld;
import com.apicatalog.rdf.RdfDataset;

public class RdfToJsonLdProcessor {

    
    public static final JsonArray fromRdf(final RdfDataset dataset, final JsonLdOptions options) throws JsonLdError {
        
        return RdfToJsonld.with(dataset)
                //TODO set options
                            .build();
    }
}
