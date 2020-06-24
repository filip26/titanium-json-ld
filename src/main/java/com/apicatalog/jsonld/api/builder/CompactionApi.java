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
import com.apicatalog.jsonld.uri.UriUtils;

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

    /**
     * Override an existing settings with {@link JsonLdOptions}.
     * 
     * @param options {@link JsonLdOptions}
     * @return {@link CompactionApi} instance 
     */
    public CompactionApi options(JsonLdOptions options) {
        
        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }
        
        this.options = options;
        return this;
    }

    /**
     * Set <code>JSON-LD</code> processing mode. JSON-LD 1.1 is set by default.
     *  
     * @param processingMode
     * @return {@link CompactionApi} instance
     */
    public CompactionApi mode(Version processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    /**
     * Set the base <code>IRI</code>. If set, this overrides the input document's IRI.
     * 
     * @param baseUri
     * @return {@link CompactionApi} instance
     */
    public CompactionApi base(URI baseUri) {        
        options.setBase(baseUri);
        return this;
    }

    /**
     * Set the base {@link URI}. If set, this overrides the input document's IRI.
     *
     * @param baseUri
     * @return {@link CompactionApi} instance
     */
    public CompactionApi base(String baseUri) {
        return base(baseUri != null ? UriUtils.create(baseUri) : null);
    }

    
    /**
     * If set to <code>true</code>, the processor replaces arrays with just one
     * element  If set to false, all arrays will remain arrays even if they have just one
     * element.
     *
     * @param enable 
     * @return {@link CompactionApi} instance
     */
    public CompactionApi compactArrays(boolean enable) {
        options.setCompactArrays(enable);
        return this;
    }

    /**
     * The processor replaces arrays with just one element. 
     * 
     * @return {@link CompactionApi} instance
     */
    public CompactionApi compactArrays() {
        return compactArrays(true);
    }
    
    /**
     * Determines if IRIs are compacted relative to the {@link #base(URI)} or document location 
     * 
     * @param enable
     * @return {@link CompactionApi} instance
     */
    public CompactionApi compactToRelative(boolean enable) {
        options.setCompactToRelative(enable);
        return this;
    }

    /**
     * IRIs are compacted relative to the {@link #base(URI)} or document location. 
     * 
     * @return {@link CompactionApi} instance
     */
    public CompactionApi compactToRelative() {
        return compactToRelative(true);
    }

    /**
     * Set the loader to be used to retrieve remote documents and
     * contexts, implementing the {@link LoadDocumentCallback}. If specified, it is
     * used to retrieve remote documents and contexts; otherwise, if not specified,
     * the processor's built-in loader is used.
     * 
     * @param loader
     * @return {@link CompactionApi} instance
     */
    public CompactionApi loader(LoadDocumentCallback loader) {        
        options.setDocumentLoader(loader);
        return this;
    }
    
    /**
     * If set to <code>true</code>, certain algorithm processing steps where
     * indicated are ordered lexicographically. If <code>false</code>, order is not
     * considered in processing.
     * 
     * @param enable
     * @return {@link CompactionApi} instance
     */
    public CompactionApi ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    /**
     * Certain algorithm processing steps where indicated are ordered lexicographically.
     * 
     * @return {@link CompactionApi} instance
     */
    public CompactionApi ordered() {
        return ordered(true);
    }

    /**
     * Get the result of compaction.
     * 
     * @return {@link JsonObject} representing a result of compaction
     * @throws JsonLdError
     */
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
