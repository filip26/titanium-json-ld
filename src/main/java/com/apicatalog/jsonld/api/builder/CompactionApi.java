package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.CompactionProcessor;

public final class CompactionApi {

    // required
    private final RemoteDocument document;
    private final URI documentUri;
    private final RemoteDocument context;
    private final URI contextUri;
    
    // optional
    private JsonLdOptions options;
    
    public CompactionApi(URI documentUri, JsonStructure context) {
        this.document = null;
        this.documentUri = documentUri;
        this.context = RemoteDocument.of(context);
        this.contextUri = null;
        this.options = new JsonLdOptions();
    }

    public CompactionApi(URI documentUri, URI contextUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.context = null;
        this.contextUri = contextUri;
        this.options = new JsonLdOptions();
    }

    public CompactionApi(RemoteDocument document, RemoteDocument context) {
        this.document = document;
        this.documentUri = null;
        this.context = context;
        this.contextUri = null;
        this.options = new JsonLdOptions();
    }

    public CompactionApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }
        
        this.options = options;
        return this;
    }

    public CompactionApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    public CompactionApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    public CompactionApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    public CompactionApi compactArrays(boolean enable) {
        options.setCompactArrays(enable);
        return this;
    }

    public CompactionApi compactArrays() {
        return compactArrays(true);
    }

    public CompactionApi compactToRelative(boolean enable) {
        options.setCompactToRelative(enable);
        return this;
    }

    public CompactionApi compactToRelative() {
        return compactToRelative(true);
    }

    public CompactionApi loader(LoadDocumentCallback loader) {        
        options.setDocumentLoader(loader);
        return this;
    }

    public CompactionApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    public CompactionApi ordered() {
        return ordered(true);
    }

    public JsonObject get() throws JsonLdError {
        if (documentUri != null && contextUri != null)  {
            return CompactionProcessor.compact(documentUri, contextUri, options);
        }        
        if (documentUri != null && context != null)  {
            return CompactionProcessor.compact(documentUri, context, options);
        }
        if (document != null && context != null)  {
            return CompactionProcessor.compact(document, context, options);
        }
        throw new IllegalStateException();
    }
}
