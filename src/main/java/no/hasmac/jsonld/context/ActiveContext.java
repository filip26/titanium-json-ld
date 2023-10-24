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
package no.hasmac.jsonld.context;

import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.compaction.UriCompaction;
import no.hasmac.jsonld.compaction.ValueCompaction;
import no.hasmac.jsonld.expansion.UriExpansion;
import no.hasmac.jsonld.expansion.ValueExpansion;
import no.hasmac.jsonld.lang.DirectionType;
import jakarta.json.JsonObject;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 */
public final class ActiveContext {

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
    private DirectionType defaultBaseDirection;

    private final JsonLdOptions options;

    // a cache of prefixes that can be iterated through for quick lookup
    private final List<CachedPrefix> prefixCache = new ArrayList<>(10);

    public ActiveContext(final JsonLdOptions options) {
        this(null, null, null, options);
    }

    public ActiveContext(final URI baseUri, final URI baseUrl, JsonLdOptions options) {
        this(baseUri, baseUrl, null, options);
    }

    public ActiveContext(final URI baseUri, final URI baseUrl, final ActiveContext previousContext, final JsonLdOptions options) {
        this.baseUri = baseUri;
        this.baseUrl = baseUrl;
        this.previousContext = previousContext;
        this.terms = new LinkedHashMap<>();
        this.options = options;
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
        this.options = origin.options;
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

    public Optional<TermDefinition> getTerm(final String value) {
        if (value == null) {
            return Optional.empty();
        }
        return Optional.ofNullable(terms.get(value));
    }

    public TermDefinition getTermNullable(final String value) {
        if (value == null) {
            return null;
        }
        return terms.get(value);
    }

    public DirectionType getDefaultBaseDirection() {
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

    public boolean inMode(final JsonLdVersion version) {
        return options.getProcessingMode() != null && options.getProcessingMode().equals(version);
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
        return UriExpansion.with(this).uriValidation(options.isUriValidation());
    }

    public ValueExpansion valueExpansion() {
        return ValueExpansion.with(this);
    }

    public UriCompaction uriCompaction() {
        return UriCompaction.with(this);
    }

    public ValueCompaction valueCompaction() {
        return ValueCompaction.with(this);
    }

    public TermDefinitionBuilder newTerm(final JsonObject localContext, final Map<String, Boolean> defined) {
        return TermDefinitionBuilder.with(this, localContext, defined);
    }

    public TermSelector termSelector(final String variable, final Collection<String> containerMapping, final String typeLanguage) {
        return TermSelector.with(this, variable, containerMapping, typeLanguage);
    }

    public JsonLdOptions getOptions() {
        return options;
    }

    protected void setDefaultBaseDirection(final DirectionType defaultBaseDirection) {
        this.defaultBaseDirection = defaultBaseDirection;
    }

    protected void setDefaultLanguage(final String defaultLanguage) {
        this.defaultLanguage = defaultLanguage;
    }

    protected void setVocabularyMapping(final String vocabularyMapping) {
        this.vocabularyMapping = vocabularyMapping;
    }

    protected void setBaseUrl(final URI baseUrl) {
        this.baseUrl = baseUrl;
    }

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
        return "ActiveContext[terms=" + terms + ", previousContext=" + previousContext + "]";
    }

    public TermDefinition getPrefix(String prefix) {
        if (prefix == null) {
            return null;
        }
        if (prefixCache.size() <= 10) {
            for (CachedPrefix cachedPrefix : prefixCache) {
                if (cachedPrefix.prefix.equals(prefix)) {
                    return cachedPrefix.termDefinition;
                }
            }
        } else {
            prefixCache.clear();
        }

        TermDefinition termDefinition = terms.get(prefix);
        if (termDefinition != null) {
            prefixCache.add(new CachedPrefix(prefix, termDefinition));
        }
        return termDefinition;


    }

    private static class CachedPrefix {
        private final String prefix;
        private final TermDefinition termDefinition;

        private CachedPrefix(String prefix, TermDefinition termDefinition) {
            this.prefix = prefix;
            this.termDefinition = termDefinition;
        }
    }
}
