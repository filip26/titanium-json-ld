package com.apicatalog.jsonld.api.builder;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.CompactionProcessor;

public final class CompactionApi {

    // required
    private final URI document;
    private final JsonStructure jsonContext;
    private final URI contextUri;
    
    // optional
    private JsonLdOptions options;
    
    public CompactionApi(URI document, JsonStructure context) {
        this.document = document;
        this.jsonContext = context;
        this.contextUri = null;
        this.options = new JsonLdOptions();
    }

    public CompactionApi(URI document, URI contextUri) {
        this.document = document;
        this.jsonContext = null;
        this.contextUri = contextUri;
        this.options = new JsonLdOptions();
    }

    public CompactionApi options(JsonLdOptions options) {
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

    public CompactionApi compactArray(boolean enable) {
        options.setCompactArrays(enable);
        return this;
    }

    public CompactionApi compactArray() {
        return compactArray(true);
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
        if (jsonContext != null) {
            return CompactionProcessor.compact(document, jsonContext, options);
        }
        if (contextUri != null) {
            return CompactionProcessor.compact(document, jsonContext, options);
        }
        throw new IllegalStateException();
    }
}
