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
package com.apicatalog.jsonld.context;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.compaction.UriCompaction;
import com.apicatalog.jsonld.compaction.ValueCompaction;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.expansion.ValueExpansion;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.processor.ProcessingRuntime;
import com.apicatalog.tree.io.NodeAdapter;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 *
 */
public final class ActiveContext implements Context {

    // the active term definitions which specify how keys and values have to be
    // interpreted
    private final Map<String, TermDefinition> terms;

    // the current base IRI
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

    private final ProcessingRuntime runtime;

    public ActiveContext(final ProcessingRuntime runtime) {
        this(null, null, null, runtime);
    }

    public ActiveContext(final URI baseUri, final URI baseUrl, ProcessingRuntime runtime) {
        this(baseUri, baseUrl, null, runtime);
    }

    public ActiveContext(final URI baseUri, final URI baseUrl, final ActiveContext previousContext, final ProcessingRuntime runtime) {
        this.baseUri = baseUri;
        this.baseUrl = baseUrl;
        this.previousContext = previousContext;
        this.terms = new LinkedHashMap<>();
        this.runtime = runtime;
    }

    // copy constructor
    public ActiveContext(final ActiveContext origin) {
        this.terms = new LinkedHashMap<>(origin.terms);
        this.baseUri = origin.baseUri;
        this.baseUrl = origin.baseUrl;
        this.inverseContext = origin.inverseContext;
        this.previousContext = origin.previousContext;
        this.vocabularyMapping = origin.vocabularyMapping;
        this.defaultLanguage = origin.defaultLanguage;
        this.defaultBaseDirection = origin.defaultBaseDirection;
        this.runtime = origin.runtime;
    }

    public void createInverseContext() {
        this.inverseContext = InverseContextBuilder.with(this).build();
    }

    public boolean containsTerm(final String term) {
        return terms.containsKey(term);
    }

    public boolean containsProtectedTerm() {
        return terms.values().stream().anyMatch(TermDefinition::isProtected);
    }

    protected Optional<TermDefinition> removeTerm(final String term) {
        if (terms.containsKey(term)) {
            return Optional.of(terms.remove(term));
        }
        return Optional.empty();
    }

    public Optional<TermDefinition> findTerm(final String value) {
        return Optional.ofNullable(terms.get(value));
    }

    public Direction getDefaultBaseDirection() {
        return defaultBaseDirection;
    }

    public String getDefaultLanguage() {
        return defaultLanguage;
    }

    public URI getBaseUri() {
        return baseUri;
    }

    public String getVocabularyMapping() {
        return vocabularyMapping;
    }

    public ActiveContext getPreviousContext() {
        return previousContext;
    }

    public URI getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUri(final URI baseUri) {
        this.baseUri = baseUri;
    }

    public InverseContext getInverseContext() {
        return inverseContext;
    }

    public Map<String, TermDefinition> getTermsMapping() {
        return terms;
    }

    public Collection<String> getTerms() {
        return terms.keySet();
    }

    public ActiveContextBuilder newContext() {
        return ActiveContextBuilder.with(this);
    }

    public UriExpansion uriExpansion() {
        return UriExpansion.with(this).uriValidation(runtime.getUriValidation());
    }

    public Map<String, ?> expandValue(final String activeProperty, final Object value, final NodeAdapter adapter) throws JsonLdError, IOException {
        return ValueExpansion.expand(this, activeProperty, value, adapter);
    }

    public Object compactValue(final Map<String, ?> value, final String activeProperty) throws JsonLdError {
        return ValueCompaction.compact(this, value, activeProperty);
    }

    @Override
    public String compactUri(String variable) throws JsonLdError {
        return UriCompaction.compact(this, variable, null, false, false);
    }

    @Override
    public String compactUriWithVocab(String variable) throws JsonLdError {
        return UriCompaction.compactWithVocab(this, variable);
    }

    public TermDefinitionBuilder newTerm(final Object localContext, final NodeAdapter adapter, final Map<String, Boolean> defined) {
        return TermDefinitionBuilder.with(this, localContext, adapter, defined);
    }

    public Optional<String> selectTerm(
            final Collection<String> preferredValues,
            final String variable,
            final Collection<String> containerMapping,
            final String typeLanguage) {
        return TermSelector.match(preferredValues, this, variable, containerMapping, typeLanguage);
    }

    protected void setDefaultBaseDirection(final Direction defaultBaseDirection) {
        this.defaultBaseDirection = defaultBaseDirection;
    }

    protected void setDefaultLanguage(final String defaultLanguage) {
        this.defaultLanguage = defaultLanguage;
    }

    protected void setVocabularyMapping(final String vocabularyMapping) {
        this.vocabularyMapping = vocabularyMapping;
    }

//    protected void setBaseUrl(final URI baseUrl) {
//        this.baseUrl = baseUrl;
//    }

    protected void setPreviousContext(final ActiveContext previousContext) {
        this.previousContext = previousContext;
    }

    protected void setInverseContext(final InverseContext inverseContext) {
        this.inverseContext = inverseContext;
    }

    protected void setTerm(final String term, final TermDefinition definition) {
        terms.put(term, definition);
    }

    @Override
    public String toString() {
        return "ActiveContext[uri=" + baseUri
                + ", terms=" + terms
                + ", previousContext=" + previousContext + "]";
    }

    public ProcessingRuntime runtime() {
        return runtime;
    }
}
