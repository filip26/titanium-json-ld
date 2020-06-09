package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;

public final class FramingApi {

    // required
    private final URI documentUri;
    private final URI frameUri;
    
    // optional
    private JsonLdOptions options;
    
    public FramingApi(URI documentUri, URI frameUri) {
        this.documentUri = documentUri;
        this.frameUri = frameUri;
        this.options = new JsonLdOptions();
    }

    public FramingApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    public FramingApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    public FramingApi context(String contextUri) {
        return context(URI.create(contextUri));
    }

    public FramingApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public FramingApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public FramingApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public FramingApi frame(boolean enable) {
        options.setFrameExpansion(enable);
        return this;
    }

    public FramingApi frame() {
        return frame(true);
    }

    public FramingApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    public FramingApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    public FramingApi ordered() {
        return ordered(true);
    }
    
    public JsonObject get() throws JsonLdError {        
        return null;
    }
}
