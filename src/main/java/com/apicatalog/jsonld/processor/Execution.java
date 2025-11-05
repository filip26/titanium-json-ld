package com.apicatalog.jsonld.processor;

import java.util.Optional;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    //TODO builder, to set collectors, like type map
    
    public static Execution of(Options options) {
        return options.timeout() != null
                ? new Ticker(options)
                : new Execution();  //TODO set empty instance
    }

    /**
     * Called in multiple places during a processing to check processing timeout if
     * set. Does nothing if timeout is not set.
     * 
     * When hit for the first time a timestamp is set, otherwise a duration is
     * decreased by timestamps difference.
     * 
     * @throws JsonLdException if a processing has exceeded
     */
    public void tick() throws JsonLdException {
        /* NOP does nothing if timeout is not set */}

    public Optional<TypeMapCollector> typeMapper() {
        return Optional.empty();
    }

//    /**
//     * Resume ticker, a next ping decreases remaining time if timeout is set. Is
//     * used after an external method call, to exclude time consumed by
//     * the external call. e.g. when calling HTTP client.
//     * 
//     * Does nothing if timeout is not set.
//     */
//    public void start() {/* NOP does nothing if timeout is not set */}
//
//    public UriValidationPolicy getUriValidation() {
//        return options.getUriValidation();
//    }
//
////    public boolean isV10() {
////        return options.getProcessingMode() != null && options.getProcessingMode().equals(JsonLdVersion.V1_0);
////    }
////
////    public boolean isV11() {
////        return options.getProcessingMode() != null && options.getProcessingMode().equals(JsonLdVersion.V1_1);
////    }
//
//    public DocumentLoader getDocumentLoader() {
//        return options.loader();
//    }
//
//    public Cache<String, JsonValue> getContextCache() {
//        return options.getContextCache();
//    }
//
//    public Cache<String, Document> getDocumentCache() {
//        return options.getDocumentCache();
//    }
//
//    public boolean isRdfStar() {
//        return options.isRdfStar();
//    }
//
//    public boolean isNumericId() {
//        return options.isNumericId();
//    }
//
//    public ProcessingPolicy getUndefinedTermPolicy() {
//        return options.undefinedTermsPolicy();
//    }
//    
//    @Deprecated
//    public JsonLdVersion version() {
//        return options.getProcessingMode();
//    }
//
//    public boolean ordered() {
//        return options.isOrdered();
//    }
}
