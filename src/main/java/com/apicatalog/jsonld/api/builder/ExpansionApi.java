package com.apicatalog.jsonld.api.builder;

import java.io.Writer;
import java.net.URI;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.processor.ExpansionProcessor;

public final class ExpansionApi {

    // required
    private final URI document;
    
    // optional
    private JsonLdOptions options;
    
    public ExpansionApi(URI document) {
        this.document = document;
        this.options = new JsonLdOptions();
    }
    
    public ExpansionApi options(JsonLdOptions options) {
        this.options = options;
        return this;
    }
    
    public ExpansionApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }
    
    public ExpansionApi mode(Version version) {
        options.setProcessingMode(version);
        return this;
    }
    
    public JsonArray get() throws JsonLdError {
        return ExpansionProcessor.expand(document, options);
    }
    
    public void toJson(Writer writer) {
        throw new UnsupportedOperationException();
    }
    
}
