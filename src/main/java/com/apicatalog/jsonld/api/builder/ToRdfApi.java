package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.JsonLdToRdfProcessor;
import com.apicatalog.rdf.RdfDataset;

public final class ToRdfApi {

    // required
    private final URI document;
    
    // optional
    private JsonLdOptions options;
    
    public ToRdfApi(URI document) {
        this.document = document;
        this.options = new JsonLdOptions();
    }
    
    public ToRdfApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    public ToRdfApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    public ToRdfApi context(String contextUri) {
        return context(URI.create(contextUri));
    }

    public ToRdfApi produceGeneralizedRdf(RdfDirection rdfDirection) {
        options.setRdfDirection(rdfDirection);
        return this;
    }

    public ToRdfApi produceGeneralizedRdf() {
        return produceGeneralizedRdf(true);
    }

    public ToRdfApi produceGeneralizedRdf(boolean enable) {
        options.setProduceGeneralizedRdf(enable);
        return this;
    }

    public ToRdfApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public ToRdfApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public ToRdfApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public ToRdfApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    public ToRdfApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    public ToRdfApi ordered() {
        return ordered(true);
    }
    
    public RdfDataset get() throws JsonLdError {
        return JsonLdToRdfProcessor.toRdf(document, options);
    }
}
