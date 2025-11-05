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

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.TreeIOAdapter;
import com.apicatalog.tree.io.TreeIO;

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

    private TreeIO source;
    
//    private final ProcessingRuntime runtime;

    ActiveContext(final Version version) {
        this(null, null, null, version);
    }

    public ActiveContext(final URI baseUri, final URI baseUrl, Version version) {
        this(baseUri, baseUrl, null, version);
    }

    ActiveContext(final URI baseUri, final URI baseUrl, final ActiveContext previousContext, final Version version) {
        this.baseUri = baseUri;
        this.baseUrl = baseUrl;
        this.previousContext = previousContext;
        this.terms = new LinkedHashMap<>();
//        this.runtime = runtime;
        this.version = version;
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
//        this.runtime = origin.runtime;
        this.version = origin.version;
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

    @Override
    public String getDefaultLanguage() {
        return defaultLanguage;
    }

    @Override
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

    @Override
    public ContextBuilder newContext(DocumentLoader loader) {
        return ContextBuilder.with(this, loader);
    }

    @Override
    public UriExpansion uriExpansion(DocumentLoader loader) {
        return UriExpansion.with(this, loader);
//FIXME                .uriValidation(
//                runtime.getUriValidation()
//                );
    }

//    public Map<String, ?> expandValue(final String activeProperty, final Object value, final NodeAdapter adapter) throws JsonLdError, IOException {
//        return ValueExpansion.expand(this, activeProperty, value, adapter, runtime);
//    }

    @Override
    public TermDefinitionBuilder newTerm(final Object localContext, final TreeIOAdapter adapter, final Map<String, Boolean> defined, DocumentLoader loader) {
        return TermDefinitionBuilder.with(this, localContext, adapter, defined, loader);
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

    @Override
    public Version version() {
        return version;
    }

    public void setVersion(Version version) {
        this.version = version;
    }
    
    public void setSource(TreeIO source) {
        this.source = source;
    }
    
    @Override
    public TreeIO source() {
        return source;
    }
}
