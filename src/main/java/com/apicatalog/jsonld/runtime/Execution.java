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
import com.apicatalog.jsonld.Options;

/**
 * A runtime execution events fired during a transformation processing.
 * 
 * @since 1.4.0
 */
public class Execution {

    public enum EventType {
        onBeginMap,
        onEndMap,
        onBeginList,
        onEndList,
        onTypeKey,
        onTermKey,

        onUndefinedTerm,
        onDroppedValue,
    }

    @FunctionalInterface
    public interface EventProcessor {
        void onEvent(EventType type, String key, String value) throws JsonLdException;
    }

    private final Collection<EventProcessor> listeners;
    private Consumer<Collection<String>> onContextKey;

    protected Execution(
            Collection<EventProcessor> listeners,
            Consumer<Collection<String>> contextKeyCollector) {
        this.listeners = listeners;
        this.onContextKey = contextKeyCollector;
    }

    public static Execution of(Options options, EventProcessor... listeners) {

        // TODO optimize
//        if (listeners.length == 0) {
//            return new Execution(List.of(), null);
//        }

        final var consumers = new ArrayList<EventProcessor>((listeners == null ? 0 : listeners.length) + 1);

        if (options.timeout() != null) {
            consumers.add(new TimeLimiter(options.timeout())::onEvent);
        }

        return new Execution(consumers, null);
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

    public void fire(EventType type, String key) throws JsonLdException {
        for (final var consumer : listeners) {
            consumer.onEvent(type, key, null);
        }
    }

    public void fire(EventType type, String key, String uri) throws JsonLdException {
        for (final var consumer : listeners) {
            consumer.onEvent(type, key, uri);
        }
    }

    /**
     * Event fired when a new context key is encountered.
     * 
     * use beginMap(@context)
     * 
     * @param keys
     */
    public void contextKeys(Collection<String> keys) {
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
}
