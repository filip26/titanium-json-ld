package com.apicatalog.jsonld.processor;

import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    @FunctionalInterface
    public interface NodeCounter {
        void increment() throws JsonLdException;
    }

    protected Consumer<String> contextKeyCollector;
    protected NodeCounter counter;

    protected Execution(NodeCounter counter, Consumer<String> contextKeyCollector) {
        this.counter = counter;
        this.contextKeyCollector = contextKeyCollector;
    }

    public static Execution of(Options options) {
        return new Execution(
                options.timeout() != null
                        ? new Ticker(options.timeout())::tick
                        : () -> {
                        },
                null);
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
    @Deprecated
    public void tick() throws JsonLdException {
        if (counter != null) {
            counter.increment();
        }
    }

    /**
     * Event fired before a node is processed.
     * 
     * @param parentKey
     */
    public void onBeforeMap(String parentKey) throws JsonLdException {
        if (counter != null) {
            counter.increment();
        }
    }

    /**
     * Event fired after a node is processed.
     * 
     * @param parentKey
     */
    public void onAfterMap(String parentKey) throws JsonLdException {
        // hook for extensions or instrumentation
    }

    public void onTypeKey(String type) throws JsonLdException {

    }

    /**
     * Event fired when a new context key is encountered.
     * 
     * @param key
     */
    public void onContextKey(String key) {
        if (contextKeyCollector != null) {
            contextKeyCollector.accept(key);
        }
    }

    public TypeMapCollector typeMapper() {
        return null;
    }

    public Consumer<String> contextKeyCollector() {
        return contextKeyCollector;
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
