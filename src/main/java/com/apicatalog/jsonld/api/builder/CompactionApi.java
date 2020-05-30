package com.apicatalog.jsonld.api.builder;

import java.io.Writer;
import java.net.URI;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.processor.CompactionProcessor;

public final class CompactionApi {

    // required
    private final URI document;
    private final JsonLdContext context;
    
    // optional
    private JsonLdOptions options;
    
    public CompactionApi(URI document, JsonLdContext context) {
        this.document = document;
        this.context = context;
        this.options = new JsonLdOptions();
    }
    
    public CompactionApi options(JsonLdOptions options) {
        this.options = options;
        return this;
    }
    
    public CompactionApi mode(Version version) {
        options.setProcessingMode(version);
        return this;
    }
        
    public JsonObject get() throws JsonLdError {
        return CompactionProcessor.compact(document, context, options);
    }
    
    public void toJson(Writer writer) {
        throw new UnsupportedOperationException();
    }
}
