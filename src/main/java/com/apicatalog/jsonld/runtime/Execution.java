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
package com.apicatalog.jsonld.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;

/**
 * A runtime execution events fired during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    @FunctionalInterface
    @Deprecated
    public interface Counter {

        /**
         * Increments the node count by one.
         *
         * @throws com.apicatalog.jsonld.JsonLdException
         */
        void increment() throws JsonLdException;
    }

    public enum EventType {
        onBeginMap,
        onEndMap,
        onBeginList,
        onEndList,
        onTypeKey,
        onTermKey,
    }

    @FunctionalInterface
    public interface EventProcessor {
        void onEvent(EventType type, String key, String value) throws JsonLdException;
    }

    private Collection<EventProcessor> listeners;
    private Consumer<Collection<String>> onContextKey;

    private Counter nodeCounter;

    protected Execution(Consumer<Collection<String>> contextKeyCollector) {
        this.onContextKey = contextKeyCollector;
    }

    public static Execution of(Options options) {
        final var exec = new Execution(null);

        if (options.timeout() != null) {
            exec.add(new TimeLimiter(options.timeout())::onEvent);
        }

        return exec;
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
//        if (ttl != null) {
//            ttl.increment();
//        }
    }

    @Deprecated
    public Execution start() throws JsonLdException {
//        if (ttl != null) {
//            ttl.increment();
//        }
        return this;
    }

    /**
     * Event fired before a map node is processed.
     * 
     * @param key
     */
    public void beginMap(String key) throws JsonLdException {

        if (nodeCounter != null) {
            nodeCounter.increment();
        }
        if (listeners != null) {
            for (final var consumer : listeners) {
                consumer.onEvent(EventType.onBeginMap, key, null);
            }
        }
    }

    /**
     * Event fired after a map node is processed.
     * 
     * @param key
     */
    public void endMap(String key) throws JsonLdException {
        // hook for extensions or instrumentation
        if (listeners != null) {
            for (final var consumer : listeners) {
                consumer.onEvent(EventType.onEndMap, key, null);
            }
        }
    }

    public void beginList(String key) throws JsonLdException {
        if (listeners != null) {
            for (final var consumer : listeners) {
                consumer.onEvent(EventType.onBeginList, key, null);
            }
        }
    }

    public void endList(String key) throws JsonLdException {
        if (listeners != null) {
            for (final var consumer : listeners) {
                consumer.onEvent(EventType.onEndList, key, null);
            }
        }
    }

    public void term(String key, String uri) throws JsonLdException {
        if (listeners != null) {
            for (final var consumer : listeners) {
                consumer.onEvent(EventType.onTermKey, key, uri);
            }
        }
    }

    public void type(String key, String type) throws JsonLdException {
        if (listeners != null) {
            for (final var listener : listeners) {
                listener.onEvent(EventType.onTypeKey, key, type);
            }
        }
    }

    public Execution add(EventProcessor listener) {
        if (listeners == null) {
            listeners = new ArrayList<>();
        }
        listeners.add(listener);
        return this;
    }

    /**
     * Event fired when a new context key is encountered.
     * 
     * use beginMap(@context)
     * 
     * @param keys
     */
    public void onContextKeys(Collection<String> keys) {
        if (onContextKey != null) {
            onContextKey.accept(keys);
        }
    }

    public boolean collectsContextKeys() {
        return onContextKey != null;
    }

    public Execution contextKeyCollector(Consumer<Collection<String>> consumer) {
        this.onContextKey = consumer;
        return this;
    }

    public Execution undefinedTermCollector(Consumer<String> collector) {

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

//    /**
//     * Resume ticker, a next ping decreases remaining time if timeout is set. Is
//     * used after an external method call, to exclude time consumed by
//     * the external call. e.g. when calling HTTP client.
//     * 
//     * Does nothing if timeout is not set.
//     */
//    public void start() {/* NOP does nothing if timeout is not set */}
}
