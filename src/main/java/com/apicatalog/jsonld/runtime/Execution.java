/*
 * Copyright 2020-2026 the original author or authors.
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
import java.util.List;
import java.util.function.Consumer;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;

/**
 * A runtime execution events fired during a transformation processing.
 * 
 * @since 2.0.0
 */
public class Execution {

    public enum EventType {
        BEGIN_MAP,
        END_MAP,
        BEGIN_LIST,
        END_LIST,

        NEXT_ELEMENT, // TODO

        TYPE_KEY,
        TERM_KEY,

        UNDEFINED_TERM,
        DROPPED_NODE,
    }

    @FunctionalInterface
    public interface EventConsumer {
        void term(EventType type, String term, Object value) throws JsonLdException;
    }

    private Collection<EventConsumer> keyValueConsumer;
    private Consumer<Collection<String>> contextKeysCollector;

    protected Execution(
            Collection<EventConsumer> keyValueConsumer,
            Consumer<Collection<String>> contextKeysCollector) {
        this.keyValueConsumer = keyValueConsumer;
        this.contextKeysCollector = contextKeysCollector;
    }

    public static Execution of(Options options) {

        if (options.timeout() != null
                || options.droppedNodes() != null
                || options.undefinedTerms() != null) {

            final var consumers = new ArrayList<EventConsumer>();

            if (options.timeout() != null) {
                consumers.add(TimeLimiter.of(options.timeout())::term);
            }
            if (options.droppedNodes() != null || options.undefinedTerms() != null) {
                consumers.add(new PolicyEnforcer(
                        options.undefinedTerms(),
                        options.droppedNodes())::term);
            }
            return new Execution(consumers, null);
        }

        return new Execution(List.of(), null);
    }

    public Execution add(EventConsumer consumer) {
        if (keyValueConsumer.isEmpty()) {
            keyValueConsumer = new ArrayList<>();
        }
        keyValueConsumer.add(consumer);
        return this;
    }

    public Execution contextKeysCollector(Consumer<Collection<String>> consumer) {
        this.contextKeysCollector = consumer;
        return this;
    }

    public void fireEvent(EventType type, String key) throws JsonLdException {
        for (final var consumer : keyValueConsumer) {
            consumer.term(type, key, null);
        }
    }

    public void fireEvent(EventType type, String key, Object uri) throws JsonLdException {
        for (final var consumer : keyValueConsumer) {
            consumer.term(type, key, uri);
        }
    }

    public void acceptContextKeys(Collection<String> values) {
        contextKeysCollector.accept(values);
    }

    public boolean hasContextKeysCollector() {
        return contextKeysCollector != null;
    }

    public boolean hasEventConsumer() {
        return !keyValueConsumer.isEmpty();
    }
}
