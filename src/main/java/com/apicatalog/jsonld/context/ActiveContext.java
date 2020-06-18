package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.compaction.UriCompactionBuilder;
import com.apicatalog.jsonld.compaction.ValueCompactionBuilder;
import com.apicatalog.jsonld.expansion.UriExpansionBuilder;
import com.apicatalog.jsonld.expansion.ValueExpansionBuilder;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Version;

/**
 * A context that is used to resolve terms while the processing algorithm is
 * running.
 * 
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
    
    public ActiveContext(JsonLdOptions options) {
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

    protected Optional<TermDefinition> removeTerm(String term) {

        if (terms.containsKey(term)) {
            TermDefinition def = terms.get(term);
            terms.remove(term);
            return Optional.of(def);
        }

        return Optional.empty();
    }

    public Optional<TermDefinition> getTerm(String value) {
        return Optional.ofNullable(terms.get(value));
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

    public boolean inMode(Version version) {
        return options.getProcessingMode() != null && options.getProcessingMode().equals(version);
    }

    public Optional<ActiveContext> getPreviousContext() {
        return Optional.ofNullable(previousContext);
    }
    
    public URI getBaseUrl() {
        return baseUrl;
    }
    
    public void setBaseUri(URI baseUri) {
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

    public ValueExpansionBuilder valueExpansion(final JsonValue element, final String activeProperty) throws JsonLdError {
        return ValueExpansionBuilder.with(this, element, activeProperty);
    }

    public UriExpansionBuilder uriExpansion(final String value) {
        return UriExpansionBuilder.with(this, value);
    }

    public TermDefinitionBuilder newTerm(JsonObject localContext, Map<String, Boolean> defined) {
        return TermDefinitionBuilder.with(this, localContext, defined);
    }

    public ActiveContextBuilder newContext() {
        return ActiveContextBuilder.with(this);
    }

    public UriCompactionBuilder uriCompaction() {
        return UriCompactionBuilder.with(this);
    }

    public ValueCompactionBuilder valueCompaction() {
        return ValueCompactionBuilder.with(this);
    }

    public TermSelector termSelector(String variable, Collection<String> containerMapping, String typeLanguage) {
        return TermSelector.with(this, variable, containerMapping, typeLanguage);
    }

    public JsonLdOptions getOptions() {
        return options;
    }
    
    protected void setDefaultBaseDirection(DirectionType defaultBaseDirection) {
        this.defaultBaseDirection = defaultBaseDirection;
    }
    
    protected void setDefaultLanguage(String defaultLanguage) {
        this.defaultLanguage = defaultLanguage;
    }
    
    protected void setVocabularyMapping(String vocabularyMapping) {
        this.vocabularyMapping = vocabularyMapping;
    }
    
    protected void setBaseUrl(URI baseUrl) {
        this.baseUrl = baseUrl;
    }
    
    protected void setPreviousContext(ActiveContext previousContext) {
        this.previousContext = previousContext;
    }
    
    protected void setInverseContext(InverseContext inverseContext) {
        this.inverseContext = inverseContext;
    }
    
    protected void setTerm(String term, TermDefinition definition) {
        terms.put(term, definition);
    }
}