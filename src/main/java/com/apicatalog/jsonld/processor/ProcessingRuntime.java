package com.apicatalog.jsonld.processor;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.ProcessingPolicy;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.cache.Cache;
import com.apicatalog.jsonld.loader.JsonLdLoader;
import com.apicatalog.jsonld.uri.UriValidationPolicy;

import jakarta.json.JsonValue;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class ProcessingRuntime {

    protected final JsonLdOptions options;

    protected ProcessingRuntime(JsonLdOptions options) {
        this.options = options;
    }

    public static ProcessingRuntime of(JsonLdOptions options) {
        return options.getTimeout() != null
                ? new Ticker(options)
                : new ProcessingRuntime(options);
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
    public void tick() throws JsonLdError {/* NOP does nothing if timeout is not set */}

    /**
     * Resume ticker, a next ping decreases remaining time if timeout is set. Is
     * used after an external method call, to exclude time consumed by
     * the external call. e.g. when calling HTTP client.
     * 
     * Does nothing if timeout is not set.
     */
    public void resetTicker() {/* NOP does nothing if timeout is not set */}

    public UriValidationPolicy getUriValidation() {
        return options.getUriValidation();
    }

    public boolean isV10() {
        return options.getProcessingMode() != null && options.getProcessingMode().equals(JsonLdVersion.V1_0);
    }

    public boolean isV11() {
        return options.getProcessingMode() != null && options.getProcessingMode().equals(JsonLdVersion.V1_1);
    }

    public JsonLdLoader getDocumentLoader() {
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

    public ProcessingPolicy getUndefinedTermPolicy() {
        return options.getUndefinedTermsPolicy();
    }
}
