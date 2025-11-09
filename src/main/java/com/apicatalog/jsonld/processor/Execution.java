package com.apicatalog.jsonld.processor;

import java.util.Optional;
import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    protected Consumer<String> contextKeysCollector;
    
    public Execution() {
        this.contextKeysCollector = null;
    }
    
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
    
    public Consumer<String> contextKeysCollector() {
        return contextKeysCollector;
    }

//    /**
//     * Resume ticker, a next ping decreases remaining time if timeout is set. Is
//     * used after an external method call, to exclude time consumed by
//     * the external call. e.g. when calling HTTP client.
//     * 
//     * Does nothing if timeout is not set.
//     */
//    public void start() {/* NOP does nothing if timeout is not set */}
}
