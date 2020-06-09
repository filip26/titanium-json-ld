package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.processor.RdfToJsonLdProcessor;
import com.apicatalog.rdf.RdfDataset;

public final class FromRdfApi {

    // required
    private final RdfDataset dataset;
    
    // optional
    private JsonLdOptions options;
    
    public FromRdfApi(RdfDataset dataset) {
        this.dataset = dataset;
        this.options = new JsonLdOptions();
    }
    
    public FromRdfApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    public FromRdfApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public FromRdfApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public FromRdfApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public FromRdfApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    public FromRdfApi ordered() {
        return ordered(true);
    }
    
    public JsonArray get() throws JsonLdError {
        return RdfToJsonLdProcessor.fromRdf(dataset, options);
    }
}
