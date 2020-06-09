package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.FlatteningProcessor;

public final class FlatteningApi {

    // required
    private final URI document;
    
    // optional
    private JsonStructure jsonContext;
    private URI contextUri;
    private JsonLdOptions options;
    
    public FlatteningApi(URI document) {
        this.document = document;
        this.jsonContext = null;
        this.contextUri = null;
        this.options = new JsonLdOptions();
    }

    public FlatteningApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    public FlatteningApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public FlatteningApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public FlatteningApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public FlatteningApi compactArrays(boolean enable) {
        options.setCompactArrays(enable);
        return this;
    }

    public FlatteningApi compactArrays() {
        return compactArrays(true);
    }

    public FlatteningApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    public FlatteningApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    public FlatteningApi ordered() {
        return ordered(true);
    }
    
    public FlatteningApi context(URI contextUri) {
        this.contextUri = contextUri;
        return this;
    }
    
    public FlatteningApi context(String contextUri) {
        return context(URI.create(contextUri));
    }

    public FlatteningApi context(JsonStructure jsonContext) {
        this.jsonContext = jsonContext;
        return this;
    }


    public JsonStructure get() throws JsonLdError {

        if (jsonContext != null) {
            return FlatteningProcessor.flatten(document, jsonContext, options);
        }
        if (contextUri != null) {
            return FlatteningProcessor.flatten(document, jsonContext, options);
        }
        return FlatteningProcessor.flatten(document, (JsonStructure)null, options);
    }
}
