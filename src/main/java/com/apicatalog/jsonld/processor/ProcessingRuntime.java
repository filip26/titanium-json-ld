package com.apicatalog.jsonld.processor;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.context.cache.Cache;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoader;

import jakarta.json.JsonValue;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class ProcessingRuntime {

    protected final JsonLdOptions options;
    protected Duration timeout;

    protected ProcessingRuntime(JsonLdOptions options) {
        this.options = options;
        this.timeout = null;
    }

    /**
     * Called in multiple places during a processing to check processing timeout if
     * set. Does nothing if timeout is not set.
     * 
     * When hit for the first time a timestamp is set, otherwise a duration is
     * decreased by timestamps difference.
     * 
     * @throws JsonLdError if a processing has exceeded
     */
    public void ping() throws JsonLdError {

    }

    /**
     * Update remaining time and resets timestamp. Is used top stop counting
     * processing time when an external library is called, e.g. a HTTP call.
     */
    public void pause() {

    }

    public ProcessingRuntime timout(Duration timout) {
        this.timeout = timout;
        return this;
    }

    public boolean isUriValidation() {
        return options.isUriValidation();
    }

    public boolean inMode(final JsonLdVersion version) {
        return options.getProcessingMode() != null && options.getProcessingMode().equals(version);
    }

    public static ProcessingRuntime from(JsonLdOptions options) {
        return new ProcessingRuntime(options);
    }

    public DocumentLoader getDocumentLoader() {
        return options.getDocumentLoader();
    }

    public Cache<String, JsonValue> getContextCache() {
        return options.getContextCache();
    }

    public Cache<String, Document> getDocumentCache() {
        return options.getDocumentCache();
    }

    public boolean isRdfStar() {
        return options.isRdfStar();
    }

    public boolean isNumericId() {
        return options.isNumericId();
    }
}
