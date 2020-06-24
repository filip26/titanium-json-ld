package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.ExpansionProcessor;

public final class ExpansionApi {

    // required
    private final URI documentUri;
    private final RemoteDocument document;
    
    // optional
    private JsonLdOptions options;
    
    public ExpansionApi(URI documentUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.options = new JsonLdOptions();
    }

    public ExpansionApi(RemoteDocument document) {
        this.document = document;
        this.documentUri = null;
        this.options = new JsonLdOptions();
    }

    public ExpansionApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    public ExpansionApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    public ExpansionApi context(String contextUri) {
        return context(URI.create(contextUri));
    }

    public ExpansionApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public ExpansionApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public ExpansionApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public ExpansionApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    public ExpansionApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    public ExpansionApi ordered() {
        return ordered(true);
    }
    
    public JsonArray get() throws JsonLdError {
        if (document != null) {
            return ExpansionProcessor.expand(document, options, false);
            
        } else if (documentUri != null) {
            return ExpansionProcessor.expand(documentUri, options);
        }
        throw new IllegalStateException();
    }
}
