package com.apicatalog.jsonld.api.impl;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdEmbed;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.processor.FramingProcessor;
import com.apicatalog.jsonld.uri.UriUtils;

public final class FramingApi implements CommonApi<FramingApi>, LoaderApi<FramingApi>, ContextApi<FramingApi> {

    // required
    private final Document document;
    private final URI documentUri;
    private final Document frame;
    private final URI frameUri;
    
    // optional
    private JsonLdOptions options;
    
    public FramingApi(URI documentUri, URI frameUri) {
        this.document = null;
        this.documentUri = documentUri;
        this.frame = null;
        this.frameUri = frameUri;
        this.options = new JsonLdOptions();
    }

    public FramingApi(Document document, Document frame) {
        this.document = document;
        this.documentUri = null;
        this.frame = frame;
        this.frameUri = null;
        this.options = new JsonLdOptions();
    }

    @Override
    public FramingApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }
    
    @Override
    public FramingApi context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

    @Override
    public FramingApi context(String contextUri) {
        return context(contextUri != null ? UriUtils.create(contextUri) : null);
    }

    @Override
    public FramingApi context(JsonStructure context) {
        options.setExpandContext(context != null ?  JsonDocument.of(context) : null);
        return this;
    }

    @Override
    public FramingApi context(Document context) {
        options.setExpandContext(context);
        return this;
    }

    @Override
    public FramingApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public FramingApi base(URI baseUri) {
        options.setBase(baseUri);
        return this;
    }

    @Override
    public FramingApi base(String baseUri) {
        return base(URI.create(baseUri));
    }

    @Override
    public FramingApi loader(LoadDocumentCallback loader) {
        options.setDocumentLoader(loader);
        return this;
    }

    @Override
    public FramingApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }
    
    @Override
    public FramingApi ordered() {
        return ordered(true);
    }

    public FramingApi embed(JsonLdEmbed value) {
        options.setEmbed(value);
        return this;
    }

    public FramingApi explicit(boolean enable) {
        options.setExplicit(enable);
        return this;
    }

    public FramingApi explicit() {
        return explicit(true);
    }

    public FramingApi omitDefault(boolean enable) {
        options.setOmitDefault(enable);
        return this;
    }

    public FramingApi omitDefault() {
        return omitDefault(true);
    }

    public FramingApi omitGraph(boolean enable) {
        options.setOmitGraph(enable);
        return this;
    }

    public FramingApi omitGraph() {
        return omitGraph(true);
    }

    public FramingApi requiredAll(boolean enable) {
        options.setRequiredAll(enable);
        return this;
    }

    public FramingApi requiredAll() {
        return requiredAll(true);
    }
    
    /**
     * Get the result of framing.
     * 
     * @return {@link JsonObject} representing framed document
     * @throws JsonLdError
     */
    public JsonObject get() throws JsonLdError {
        if (documentUri != null && frameUri != null) {
            return FramingProcessor.frame(documentUri, frameUri, options);
        }
        if (document != null && frame != null) {
            return FramingProcessor.frame(document, frame, options);
        }

        throw new IllegalStateException();
    }
}