package com.apicatalog.jsonld.processor;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.serialization.RdfToJsonld;
import com.apicatalog.rdf.RdfDataset;

public final class RdfToJsonLdProcessor {

    private RdfToJsonLdProcessor() {
    }
    
    public static final JsonArray fromRdf(final RdfDataset dataset, final JsonLdOptions options) throws JsonLdError {
        
        return RdfToJsonld
                    .with(dataset)
                    .ordered(options.isOrdered())
                    .rdfDirection(options.getRdfDirection())
                    .useNativeTypes(options.isUseNativeTypes())
                    .useRdfType(options.isUseRdfType())
                    .processingMode(options.getProcessingMode())
                    .build();
    }
}
