/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.processor;

import java.time.Duration;
import java.util.Collection;
import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;

/**
 * A runtime execution context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    @FunctionalInterface
    public interface Counter {

        /**
         * Increments the node count by one.
         *
         * @throws com.apicatalog.jsonld.JsonLdException
         */
        void increment() throws JsonLdException;
    }

    protected Consumer<Collection<String>> contextKeyCollector;
    protected Counter nodeCounter;
    protected Counter ttl;

    protected Execution(
            Counter ttl,
            Counter nodeCounter,
            Consumer<Collection<String>> contextKeyCollector) {
        this.ttl = ttl;
        this.nodeCounter = nodeCounter;
        this.contextKeyCollector = contextKeyCollector;
    }

    public static Execution of(Options options) {
        return new Execution(
                options.timeout() != null
                        ? new Ticker(options.timeout())::tick
                        : null,
                null,
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
        if (ttl != null) {
            ttl.increment();
        }
    }

    public Execution start() throws JsonLdException {
        if (ttl != null) {
            ttl.increment();
        }
        return this;
    }

    /**
     * Event fired before a map node is processed.
     * 
     * @param parentKey
     */
    public void onBeforeMap(String parentKey) throws JsonLdException {
        if (nodeCounter != null) {
            nodeCounter.increment();
        }
    }

    /**
     * Event fired after a map node is processed.
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
    public void onContextKey(Collection<String> key) {
        System.out.println("HIT " + key);
        contextKeyCollector.accept(key);
    }

    public TypeMapCollector typeMapper() {
        return null;
    }

    public boolean collectsContextKeys() {
        return contextKeyCollector != null;
    }

    public Execution contextKeyCollector(Consumer<Collection<String>> consumer) {
        this.contextKeyCollector = consumer;
        return this;
    }

    /**
     * A counter that tracks the number of processed nodes and enforces a maximum
     * limit.
     * <p>
     * This class is typically used within JSON-LD processing to prevent excessive
     * recursion or processing of an unbounded number of nodes. When the limit is
     * exceeded, a {@link com.apicatalog.jsonld.JsonLdException} is thrown.
     * </p>
     * 
     * Example usage:
     * 
     * <pre>{@code
     * MaxNodesCounter counter = new MaxNodesCounter(1000);
     * counter.increment();
     * }</pre>
     * 
     * @since 1.4.0
     */
    static class NodeThrottle {

        private final int maxNodes;
        private int counter;

        private NodeThrottle(int maxNodes) {
            this.maxNodes = maxNodes;
            this.counter = 0;
        }

        void increment() throws JsonLdException {
            if (++counter >= maxNodes) {
                // TODO add ErrorCode.MAX_NODES_LIMIT_EXCEEDED
                throw new JsonLdException(ErrorCode.UNSPECIFIED);
            }
        }
    }

    static class Ticker {

        private final long ttl;

        private long ticker;

        private Ticker(Duration ttl) {
            this.ttl = ttl.toMillis();
            this.ticker = 0;
        }

        void tick() throws JsonLdException {

            if (ticker == 0) {
                ticker = System.currentTimeMillis();
                return;
            }

            final var now = System.currentTimeMillis();
            
            final var elapsed = ttl - (now - ticker);

            if (elapsed <= 0) {
                ticker = 0;
                throw new JsonLdException(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
            }
        }
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
