package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.FlatteningProcessor;

public final class FlatteningApi {

    // required
    private final URI document;
    private final JsonStructure jsonContext;
    private final URI contextUri;
    
    // optional
    private JsonLdOptions options;
    
    public FlatteningApi(URI document, JsonStructure context) {
        this.document = document;
        this.jsonContext = context;
        this.contextUri = null;
        this.options = new JsonLdOptions();
    }

    public FlatteningApi(URI document, URI contextUri) {
        this.document = document;
        this.jsonContext = null;
        this.contextUri = contextUri;
        this.options = new JsonLdOptions();
    }

    public FlatteningApi options(JsonLdOptions options) {
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

    public JsonStructure get() throws JsonLdError {

        if (jsonContext != null) {
            return FlatteningProcessor.flatten(document, jsonContext, options);
        }
        if (contextUri != null) {
            return FlatteningProcessor.flatten(document, jsonContext, options);
        }
        throw new IllegalStateException();
    }
}
