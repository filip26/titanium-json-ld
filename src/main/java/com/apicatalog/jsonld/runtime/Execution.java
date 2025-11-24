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
import java.util.List;

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
        
        NEXT_ELEMENT,   //TODO
        
        CONTEXT_KEYS,
        TYPE_KEY,
        TERM_KEY,        

        UNDEFINED_TERM,
        DROPPED_NODE,
    }

    @FunctionalInterface
    public interface TermValueConsumer {
        void term(EventType type, String term, String value) throws JsonLdException;
    }

    @FunctionalInterface
    public interface TermsConsumer {
        void terms(EventType type, Collection<String> terms) throws JsonLdException;
    }

    private Collection<TermValueConsumer> keyValueConsumer;
    private Collection<TermsConsumer> termsConsumer;

    protected Execution(
            Collection<TermValueConsumer> keyValueConsumer,
            Collection<TermsConsumer> valuesconsumer) {
        this.keyValueConsumer = keyValueConsumer;
        this.termsConsumer = valuesconsumer;
    }

    public static Execution of(Options options) {

        if (options.timeout() != null
                || options.droppedNodes() != null
                || options.undefinedTerms() != null) {

            final var consumers = new ArrayList<TermValueConsumer>();

            if (options.timeout() != null) {
                consumers.add(TimeLimiter.of(options.timeout())::term);
            }
            if (options.droppedNodes() != null || options.undefinedTerms() != null) {
                consumers.add(new PolicyEnforcer(
                        options.undefinedTerms(),
                        options.droppedNodes())::term);
            }
            return new Execution(consumers, List.of());
        }

        return new Execution(List.of(), List.of());
    }

    public Execution add(TermValueConsumer consumer) {
        if (keyValueConsumer.isEmpty()) {
            keyValueConsumer = new ArrayList<>();
        }
        keyValueConsumer.add(consumer);
        return this;
    }

    public Execution add(TermsConsumer consumer) {
        if (termsConsumer.isEmpty()) {
            termsConsumer = new ArrayList<>();
        }
        termsConsumer.add(consumer);
        return this;
    }

    public void fire(EventType type, String key) throws JsonLdException {
        for (final var consumer : keyValueConsumer) {
            consumer.term(type, key, null);
        }
    }

    public void fire(EventType type, String key, String uri) throws JsonLdException {
        for (final var consumer : keyValueConsumer) {
            consumer.term(type, key, uri);
        }
    }

    public void fire(EventType type, Collection<String> values) throws JsonLdException {
        for (final var consumer : termsConsumer) {
            consumer.terms(type, values);
        }
    }

    public boolean hasValuesConsumer() {
        return !termsConsumer.isEmpty();
    }
}
