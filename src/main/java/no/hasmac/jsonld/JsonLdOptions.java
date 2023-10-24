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
package no.hasmac.jsonld;

import java.net.URI;

import no.hasmac.jsonld.context.cache.Cache;
import no.hasmac.jsonld.context.cache.LruCache;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.loader.DocumentLoader;
import no.hasmac.jsonld.loader.SchemeRouter;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 * The {@link JsonLdOptions} type is used to pass various options to the processor.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#the-jsonldoptions-type">The
 *      JsonLdOptions Specification.</a>
 *
 */
public final class JsonLdOptions {

    public enum RdfDirection {
        I18N_DATATYPE,
        COMPOUND_LITERAL
    }

    /* default values */
    public static final boolean DEFAULT_RDF_STAR = false;
    public static final boolean DEFAULT_NUMERIC_ID = false;
    public static final boolean DEFAULT_URI_VALIDATION = true;

    /**
     * The base IRI to use when expanding or compacting the document.
     * If set, this overrides the input document's IRI.
     */
    private URI base;

    /**
     * If set to true, the JSON-LD processor replaces arrays with
     * just one element with that element during compaction.
     * If set to false, all arrays will remain arrays
     * even if they have just one element.
     */
    private boolean compactArrays;

    /**
     * Determines if IRIs are compacted relative to the base option
     * or document location when compacting.
     */
    private boolean compactToRelative;

    /**
     * The callback of the loader to be used to retrieve remote documents and contexts,
     *  implementing the LoadDocumentCallback. If specified, it is used to retrieve
     *  remote documents and contexts; otherwise, if not specified,
     *  the processor's built-in loader is used.
     */
    private DocumentLoader documentLoader;

    /**
     * A context that is used to initialize the active context when expanding a document.
     */
    private Document expandContext;

    private boolean extractAllScripts;

    private boolean ordered;

    private JsonLdVersion processingMode;

    private boolean produceGeneralizedRdf;

    private RdfDirection rdfDirection;

    private boolean useNativeTypes;

    private boolean useRdfType;

    // Framing https://www.w3.org/TR/json-ld11-framing/#jsonldoptions

    private JsonLdEmbed embed;

    private boolean explicit;

    private boolean omitDefault;

    private Boolean omitGraph;

    private boolean requiredAll;

    // Extension: JSON-LD-STAR (Experimental) https://json-ld.github.io/json-ld-star
    private boolean rdfStar;

    // custom

    // allow numeric @id
    private boolean numericId;

    // context cache
    private Cache<String, JsonValue> contextCache;

    // document cache
    private Cache<String, Document> documentCache;

    private boolean uriValidation;

    public JsonLdOptions() {
        this(SchemeRouter.defaultInstance());
    }

    public JsonLdOptions(DocumentLoader loader) {

        // default values
        this.base = null;
        this.compactArrays = true;
        this.compactToRelative = true;
        this.documentLoader = loader;
        this.expandContext = null;
        this.extractAllScripts = false;
        this.ordered = false;
        this.processingMode = JsonLdVersion.V1_1;
        this.produceGeneralizedRdf = true;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;

        // framing defaults
        this.embed = JsonLdEmbed.ONCE;
        this.explicit = false;
        this.omitDefault = false;
        this.omitGraph = null;
        this.requiredAll = false;

        // Extension: JSON-LD-STAR (Experimental)
        this.rdfStar = DEFAULT_RDF_STAR;

        // custom
        this.numericId = DEFAULT_NUMERIC_ID;
        this.contextCache = new LruCache<>(256);
        this.documentCache = null;
        this.uriValidation = DEFAULT_URI_VALIDATION;
    }

    public JsonLdOptions(JsonLdOptions options) {
        this.base = options.base;
        this.compactArrays = options.compactArrays;
        this.compactToRelative = options.compactToRelative;
        this.documentLoader = options.documentLoader;
        this.expandContext = options.expandContext;
        this.extractAllScripts = options.extractAllScripts;
        this.ordered = options.ordered;
        this.processingMode = options.processingMode;
        this.produceGeneralizedRdf = options.produceGeneralizedRdf;
        this.rdfDirection = options.rdfDirection;
        this.useNativeTypes = options.useNativeTypes;
        this.useRdfType = options.useRdfType;

        // framing
        this.embed = options.embed;
        this.explicit = options.explicit;
        this.omitDefault = options.omitDefault;
        this.omitGraph = options.omitGraph;
        this.requiredAll = options.requiredAll;

        // Extension: JSON-LD-STAR (Experimental)
        this.rdfStar = options.rdfStar;

        // custom
        this.numericId = options.numericId;
        this.contextCache = options.contextCache;
        this.documentCache = options.documentCache;
        this.uriValidation = options.uriValidation;
    }

    /**
     * The base IRI to use when expanding or
     * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a> the
     * document. If set, this overrides the input document's IRI.
     *
     * @return the base URI or <code>null</code>
     */
    public URI getBase() {
        return base;
    }

    /**
     * If set to <code>true</code>, the processor replaces arrays with just one
     * element with that element during
     * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compaction</a>. If
     * set to false, all arrays will remain arrays even if they have just one
     * element.
     *
     * @return <code>true</code> if array compaction is enabled
     */
    public boolean isCompactArrays() {
        return compactArrays;
    }

    /**
     * Determines if IRIs are compacted relative to the {@link #getBase()} option
     * or document location when
     * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a>.
     *
     * @return <code>true</code> if IRI relativization is enabled
     */
    public boolean isCompactToRelative() {
        return compactToRelative;
    }

    /**
     * The callback of the loader to be used to retrieve remote documents and
     * contexts, implementing the {@link DocumentLoader}. If specified, it is
     * used to retrieve remote documents and contexts; otherwise, if not specified,
     * the processor's built-in loader is used.
     *
     * @return the loader or <code>null</code> is is not set
     */
    public DocumentLoader getDocumentLoader() {
        return documentLoader;
    }

    /**
     * If set to <code>true</code>, when extracting <a href=
     * "https://www.w3.org/TR/json-ld11-api/#dfn-json-ld-script-element">JSON-LD
     * script elements</a> from HTML, unless a specific
     * <a href="https://tools.ietf.org/html/rfc3986#section-3.5">fragment
     * identifier</a> is targeted, extracts all encountered <a href=
     * "https://www.w3.org/TR/json-ld11-api/#dfn-json-ld-script-element">JSON-LD
     * script elements</a> using an array form, if necessary.
     *
     * @return <code>true</code> if script extraction is enabled
     */
    public boolean isExtractAllScripts() {
        return extractAllScripts;
    }

    /**
     * If set to <code>true</code>, certain algorithm processing steps where
     * indicated are ordered lexicographically. If <code>false</code>, order is not
     * considered in processing.
     *
     * @return <code>true</code> if array sorting is enabled
     */
    public boolean isOrdered() {
        return ordered;
    }

    public JsonLdVersion getProcessingMode() {
        return processingMode;
    }

    public boolean isProduceGeneralizedRdf() {
        return produceGeneralizedRdf;
    }

    public RdfDirection getRdfDirection() {
        return rdfDirection;
    }

    public boolean isUseNativeTypes() {
        return useNativeTypes;
    }

    public boolean isUseRdfType() {
        return useRdfType;
    }

    public Document getExpandContext() {
        return expandContext;
    }

    public void setBase(URI baseUri) {
        this.base = baseUri;
    }

    public void setCompactArrays(boolean compactArrays) {
        this.compactArrays = compactArrays;
    }

    public void setCompactToRelative(boolean compactToRelative) {
        this.compactToRelative = compactToRelative;
    }

    public void setDocumentLoader(DocumentLoader documentLoader) {
        this.documentLoader = documentLoader;
    }

    public void setExtractAllScripts(boolean extractAllScripts) {
        this.extractAllScripts = extractAllScripts;
    }

    public void setOrdered(boolean ordered) {
        this.ordered = ordered;
    }

    public void setProcessingMode(JsonLdVersion processingMode) {
        this.processingMode = processingMode;
    }

    public void setProduceGeneralizedRdf(boolean produceGeneralizedRdf) {
        this.produceGeneralizedRdf = produceGeneralizedRdf;
    }

    public void setRdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
    }

    public void setUseNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
    }

    public void setUseRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
    }

    public void setExpandContext(final String contextLocation) {

        if (contextLocation == null) {
            this.expandContext = null;
            return;
        }

        this.expandContext = JsonDocument.of(JsonProvider.instance().createArrayBuilder().add(contextLocation).build());
    }

    public void setExpandContext(URI contextUri) {

        if (contextUri == null) {
            this.expandContext = null;
            return;
        }

        setExpandContext(contextUri.toString());
    }

    public void setExpandContext(JsonObject context) {

        if (context == null) {
            this.expandContext = null;
            return;
        }

        this.expandContext = JsonDocument.of(context);
    }

    public void setExpandContext(Document context) {
        this.expandContext = context;
    }

    // Framing

    public JsonLdEmbed getEmbed() {
        return embed;
    }

    public void setEmbed(JsonLdEmbed embed) {
        this.embed = embed;
    }

    public boolean isExplicit() {
        return explicit;
    }

    public void setExplicit(boolean explicit) {
        this.explicit = explicit;
    }

    public boolean isOmitDefault() {
        return omitDefault;
    }

    public void setOmitDefault(boolean omitDefault) {
        this.omitDefault = omitDefault;
    }

    public Boolean isOmitGraph() {
        return omitGraph;
    }

    public void setOmitGraph(Boolean omitGraph) {
        this.omitGraph = omitGraph;
    }

    public boolean isRequiredAll() {
        return requiredAll;
    }

    public void setRequiredAll(boolean requiredAll) {
        this.requiredAll = requiredAll;
    }

    /**
     * Experimental: Enables/Disables numeric @id support.
     *
     * @param enable numeric @id support
     */
    public void setNumericId(boolean enable) {
        this.numericId = enable;
    }

    /**
     * Experimental: Numeric @id support state. Disabled by default.
     *
     * @return <code>true</code> if numeric @id support is enabled
     */
    public boolean isNumericId() {
        return numericId;
    }

    public Cache<String, JsonValue> getContextCache() {
        return contextCache;
    }

    public void setContextCache(Cache<String, JsonValue> contextCache) {
        this.contextCache = contextCache;
    }

    public Cache<String, Document> getDocumentCache() {
        return documentCache;
    }

    public void setDocumentCache(Cache<String, Document> documentCache) {
        this.documentCache = documentCache;
    }

    public boolean isRdfStar() {
        return rdfStar;
    }

    /**
     * Experimental: Enables JSON-LD-STAR extension. Only expansion is supported. Disabled by default.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     */
    public void setRdfStar(boolean rdfStar) {
        this.rdfStar = rdfStar;
    }

    /**
     * if disabled only URIs required for processing are parsed and validated.
     * Disabling URI validation might improve performance depending on the number of processed URIs.
     * <p>
     * <b>Warning:</b> Disabled validation could cause an invalid output.
     * </p>
     * <p>
     * Enabled by default.
     * </p>
     *
     * @return true if validation is enabled
     */
    public boolean isUriValidation() {
        return uriValidation;
    }

    /**
     * if disabled only URIs required for processing are parsed and validated.
     * Disabling URI validation might improve performance depending on the number of processed URIs.
     * <p>
     * <b>Warning:</b> Disabled validation could cause an invalid output.
     * </p>
     * <p>
     * Enabled by default.
     * </p>
     *
     * @param enabled set <code>true</code> to enable validation
     */
    public void setUriValidation(boolean enabled) {
        this.uriValidation = enabled;
    }
}
