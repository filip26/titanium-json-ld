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
package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.runtime.Execution;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 *
 */
public final class ActiveContext implements Context {

    // the active term definitions which specify how keys and values have to be
    // interpreted
    private final Map<String, TermDefinition> terms;

    private Version version;

    // the current base URI
    private URI baseUri;

    // the original base URL
    private URI baseUrl;

    private InverseContext inverseContext;

    // an optional previous context, used when a non-propagated context is defined.
    private ActiveContext previousContext;

    // an optional vocabulary mapping
    private String vocabularyMapping;

    // an optional default language
    private String defaultLanguage;

    // an optional default base direction ("ltr" or "rtl")
    private Direction defaultBaseDirection;

    ActiveContext(Version version) {
        this(null, null, null, version);
    }

    ActiveContext(URI baseUri, URI baseUrl, Version version) {
        this(baseUri, baseUrl, null, version);
    }

    ActiveContext(URI baseUri, URI baseUrl, ActiveContext previousContext, Version version) {
        this.baseUri = baseUri;
        this.baseUrl = baseUrl;
        this.previousContext = previousContext;
        this.terms = new LinkedHashMap<>();
        this.version = version;
    }

    // copy constructor
    ActiveContext(ActiveContext origin) {
        this.terms = new LinkedHashMap<>(origin.terms);
        this.baseUri = origin.baseUri;
        this.baseUrl = origin.baseUrl;
        this.inverseContext = origin.inverseContext;
        this.previousContext = origin.previousContext;
        this.vocabularyMapping = origin.vocabularyMapping;
        this.defaultLanguage = origin.defaultLanguage;
        this.defaultBaseDirection = origin.defaultBaseDirection;
        this.version = origin.version;
    }

    @Override
    public ContextBuilder builder(DocumentLoader loader, Execution runtime) {
        return ContextBuilder.with(this, loader, runtime);
    }

    @Override
    public void addTerm(
            String term,
            Map<String, ?> localContext,
            Map<String, Boolean> defined,
            DocumentLoader loader,
            Execution runtime) throws JsonLdException {
        TermDefinitionBuilder.newInstance(this, localContext, defined, loader, runtime).addTerm(term);
    }

    public InverseContext createInverseContext() {
        this.inverseContext = InverseContext.newInstance(this);
        return this.inverseContext;
    }

    public boolean containsProtectedTerm() {
        for (var term : terms.values()) {
            if (term.isProtected()) {
                return true;
            }
        }
        return false;
    }

    public boolean containsTerm(String term) {
        return terms.containsKey(term);
    }

    public Optional<TermDefinition> findTerm(String value) {
        return Optional.ofNullable(terms.get(value));
    }

    TermDefinition removeTerm(String term) {
        return terms.remove(term);
    }

    public String selectTerm(
            Collection<String> preferredValues,
            String variable,
            Collection<String> containerMapping,
            String typeLanguage) {
        return TermSelector.match(this, preferredValues, variable, containerMapping, typeLanguage);
    }

    @Override
    public String toString() {
        return "ActiveContext[uri=" + baseUri
                + ", terms=" + terms
                + ", previousContext=" + previousContext + "]";
    }

    public Direction defaultDirection() {
        return defaultBaseDirection;
    }

    @Override
    public String defaultLanguage() {
        return defaultLanguage;
    }

    @Override
    public URI baseUri() {
        return baseUri;
    }

    public String vocabularyMapping() {
        return vocabularyMapping;
    }

    public ActiveContext previousContext() {
        return previousContext;
    }

    public URI baseUrl() {
        return baseUrl;
    }

    public void baseUri(URI baseUri) {
        this.baseUri = baseUri;
    }

    public InverseContext inverseContext() {
        return inverseContext;
    }

    public Map<String, TermDefinition> termsMapping() {
        return terms;
    }

    public Collection<String> terms() {
        return terms.keySet();
    }

    void defaultBaseDirection(Direction defaultBaseDirection) {
        this.defaultBaseDirection = defaultBaseDirection;
    }

    void defaultLanguage(String defaultLanguage) {
        this.defaultLanguage = defaultLanguage;
    }

    void vocabularyMapping(String vocabularyMapping) {
        this.vocabularyMapping = vocabularyMapping;
    }

    void previousContext(ActiveContext previousContext) {
        this.previousContext = previousContext;
    }

    void inverseContext(InverseContext inverseContext) {
        this.inverseContext = inverseContext;
    }

    void term(String term, TermDefinition definition) {
        terms.put(term, definition);
    }

    @Override
    public Version version() {
        return version;
    }

    public void version(Version version) {
        this.version = version;
    }
}
