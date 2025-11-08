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
package com.apicatalog.jsonld;

import java.net.URI;
import java.time.Duration;
import java.util.Set;

import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.lang.Embed;
import com.apicatalog.jsonld.loader.CacheLoader.Cache;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.tordf.JsonLdToQuads;
import com.apicatalog.jsonld.tordf.JsonLdToQuads.RdfJsonLiteralWriter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.uri.UriValidationPolicy;

/**
 * The {@link Options} type is used to pass various options to the processor.
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#the-jsonldoptions-type">The
 *      JsonLdOptions Specification.</a>
 *
 */
public final class Options {

    public enum RdfDirection {
        I18N_DATATYPE,
        COMPOUND_LITERAL
    }

    public enum ProcessingPolicy {
        /** ignore, the current and default behavior */
        Ignore,
        /** stop processing with an error */
        Fail,
        /** print warning to log */
        Warn
    }

    /* default values */
    public static final boolean DEFAULT_RDF_STAR = false;
    public static final boolean DEFAULT_NUMERIC_ID = false;
    public static final UriValidationPolicy DEFAULT_URI_VALIDATION = UriValidationPolicy.Full;

    /**
     * The callback of the loader to be used to retrieve remote documents and
     * contexts, implementing the LoadDocumentCallback. If specified, it is used to
     * retrieve remote documents and contexts; otherwise, if not specified, the
     * processor's built-in loader is used.
     */
    private DocumentLoader loader;

    /**
     * The base IRI to use when expanding or compacting the document. If set, this
     * overrides the input document's IRI.
     */
    private URI base;

    /**
     * If set to true, the JSON-LD processor replaces arrays with just one element
     * with that element during compaction. If set to false, all arrays will remain
     * arrays even if they have just one element.
     */
    private boolean compactArrays;

    /**
     * Determines if IRIs are compacted relative to the base option or document
     * location when compacting.
     */
    private boolean compactToRelative;

    /**
     * A context that is used to initialize the active context when expanding a
     * document.
     */
    private Document expandContext;

    private boolean extractAllScripts;

    private boolean ordered;

    private Version processingMode;

    private boolean generalizedRdf;

    private RdfDirection rdfDirection;

    private boolean useNativeTypes;

    private boolean useRdfType;

    private RdfJsonLiteralWriter rdfJsonLiteralWriter;

    // Framing https://www.w3.org/TR/json-ld11-framing/#jsonldoptions

    private Embed embed;

    private boolean explicit;

    private boolean omitDefault;

    private Boolean omitGraph;

    private boolean requiredAll;

    // Extension: JSON-LD-STAR (Experimental) https://json-ld.github.io/json-ld-star
    private boolean rdfStar;

    // custom

    // allow numeric @id
    private boolean useNumericId;

    // context cache
//    private Cache<String, JsonValue> contextCache;

    // document cache
    private Cache<String, Document> documentCache;

    private UriValidationPolicy uriValidation;

    private Duration timeout;

    // a policy on how proceed with undefined terms during expansion
    private ProcessingPolicy undefinedTerms;

    private Options() {
        // default values
        this.loader = null;
        this.base = null;
        this.compactArrays = true;
        this.compactToRelative = true;
        this.expandContext = null;
        this.extractAllScripts = false;
        this.ordered = false;
        this.processingMode = Version.V1_1;
        this.generalizedRdf = true;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
        this.rdfJsonLiteralWriter = JsonLdToQuads.JCS;

        // framing defaults
        this.embed = Embed.ONCE;
        this.explicit = false;
        this.omitDefault = false;
        this.omitGraph = null;
        this.requiredAll = false;

        // Extension: JSON-LD-STAR (Experimental)
        this.rdfStar = DEFAULT_RDF_STAR;

        // custom
        this.useNumericId = DEFAULT_NUMERIC_ID;
//        this.contextCache = new LruCache<>(256);
        this.documentCache = null;
        this.uriValidation = DEFAULT_URI_VALIDATION;
        this.timeout = null;
        this.undefinedTerms = ProcessingPolicy.Ignore;
    }

    private Options(Options options) {
        this.loader = options.loader;
        this.base = options.base;
        this.compactArrays = options.compactArrays;
        this.compactToRelative = options.compactToRelative;
        this.expandContext = options.expandContext;
        this.extractAllScripts = options.extractAllScripts;
        this.ordered = options.ordered;
        this.processingMode = options.processingMode;
        this.generalizedRdf = options.generalizedRdf;
        this.rdfDirection = options.rdfDirection;
        this.useNativeTypes = options.useNativeTypes;
        this.useRdfType = options.useRdfType;
        this.rdfJsonLiteralWriter = options.rdfJsonLiteralWriter;

        // framing
        this.embed = options.embed;
        this.explicit = options.explicit;
        this.omitDefault = options.omitDefault;
        this.omitGraph = options.omitGraph;
        this.requiredAll = options.requiredAll;

        // Extension: JSON-LD-STAR (Experimental)
        this.rdfStar = options.rdfStar;

        // custom
        this.useNumericId = options.useNumericId;
//        this.contextCache = options.contextCache;
        this.documentCache = options.documentCache;
        this.uriValidation = options.uriValidation;
        this.timeout = options.timeout;
        this.undefinedTerms = options.undefinedTerms;
    }

    /**
     * Returns a new {@code JsonLdOptions} instance with default configuration.
     *
     * @return a new {@code JsonLdOptions} instance using standard defaults
     */
    public static final Options newOptions() {
        return new Options();
    }

    /**
     * Returns a new {@code JsonLdOptions} instance using the specified
     * {@link DocumentLoader}.
     * <p>
     *
     * @param loader the document loader to use; must not be {@code null}
     * @return a new {@code JsonLdOptions} instance configured with the given loader
     */
    public static final Options with(DocumentLoader loader) {
        return new Options().loader(loader);
    }

    /**
     * Returns a new {@code JsonLdOptions} instance copied from the given options.
     *
     * @param options the options to copy; must not be {@code null}
     * @return a new {@code JsonLdOptions} instance with the same configuration
     */
    public static final Options copyOf(Options options) {
        return new Options(options);
    }

    /**
     * The base IRI to use when expanding or
     * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a> the
     * document. If set, this overrides the input document's IRI.
     *
     * @return the base URI or <code>null</code>
     */
    public URI base() {
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
     * Determines if IRIs are compacted relative to the {@link #base()} option or
     * document location when
     * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a>.
     *
     * @return <code>true</code> if IRI relativization is enabled
     */
    public boolean isCompactToRelative() {
        return compactToRelative;
    }

    /**
     * The callback of the loader to be used to retrieve remote documents and
     * contexts, implementing the {@link DocumentLoader}. If specified, it is used
     * to retrieve remote documents and contexts; otherwise, if not specified, the
     * processor's built-in loader is used.
     *
     * @return the loader or <code>null</code> is is not set
     */
    public DocumentLoader loader() {
        return loader;
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

    public Version mode() {
        return processingMode;
    }

    public boolean isProduceGeneralizedRdf() {
        return generalizedRdf;
    }

    public RdfDirection rdfDirection() {
        return rdfDirection;
    }

    public boolean useNativeTypes() {
        return useNativeTypes;
    }

    public boolean useRdfType() {
        return useRdfType;
    }

    // when useJcs -> Jcs::canonize
    public RdfJsonLiteralWriter rdfJsonLiteralWriter() {
        return rdfJsonLiteralWriter;
    }

    public Document expandContext() {
        return expandContext;
    }

    public Options base(URI baseUri) {
        this.base = baseUri;
        return this;
    }

    public Options base(String baseUri) {
        this.base = URI.create(baseUri);
        return this;
    }

    public Options compactArrays(boolean compactArrays) {
        this.compactArrays = compactArrays;
        return this;
    }

    public Options compactToRelative(boolean compactToRelative) {
        this.compactToRelative = compactToRelative;
        return this;
    }

    public Options loader(DocumentLoader documentLoader) {
        this.loader = documentLoader;
        return this;
    }

    public Options extractAllScripts(boolean extractAllScripts) {
        this.extractAllScripts = extractAllScripts;
        return this;
    }

    /**
     * Sets the {@code ordered} flag.
     *
     * @param ordered if {@code true}, properties and values are processed in
     *                lexical order.
     * @return this instance, for method chaining.
     */
    public Options ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public Options mode(Version processingMode) {
        this.processingMode = processingMode;
        return this;
    }

    public Options generalizedRdf(boolean produceGeneralizedRdf) {
        this.generalizedRdf = produceGeneralizedRdf;
        return this;
    }

    public Options rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    public Options useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }

    public Options useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    // useJcs -> Jcs::canonize
    public Options rdfJsonLiteralWriter(RdfJsonLiteralWriter jsonWriter) {
        this.rdfJsonLiteralWriter = jsonWriter;
        return this;
    }

    public Options expandContext(final String contextLocation) {

        if (contextLocation == null) {
            this.expandContext = null;
            return this;
        }
        this.expandContext = Document.of(
                new TreeIO(Set.of(contextLocation), NativeAdapter.instance()));
        return this;
    }

    public Options expandContext(URI contextUri) {

        if (contextUri == null) {
            this.expandContext = null;
            return this;
        }

        return expandContext(contextUri.toString());
    }

    public Options expandContext(TreeIO node) {

        if (node == null) {
            this.expandContext = null;
            return this;
        }
        this.expandContext = Document.of(node);
        return this;
    }

    public Options expandContext(Document context) {
        this.expandContext = context;
        return this;
    }

    // Framing

    public Embed embed() {
        return embed;
    }

    public Options embed(Embed embed) {
        this.embed = embed;
        return this;
    }

    public boolean isExplicit() {
        return explicit;
    }

    public Options explicit(boolean explicit) {
        this.explicit = explicit;
        return this;
    }

    public boolean isOmitDefault() {
        return omitDefault;
    }

    public Options omitDefault(boolean omitDefault) {
        this.omitDefault = omitDefault;
        return this;
    }

    public Boolean isOmitGraph() {
        return omitGraph;
    }

    public Options omitGraph(Boolean omitGraph) {
        this.omitGraph = omitGraph;
        return this;
    }

    public boolean isRequiredAll() {
        return requiredAll;
    }

    public Options requiredAll(boolean requiredAll) {
        this.requiredAll = requiredAll;
        return this;
    }

    /**
     * Experimental: Enables/Disables numeric @id support.
     *
     * @param enable numeric @id support
     */
    public Options useNumericId(boolean enable) {
        this.useNumericId = enable;
        return this;
    }

    /**
     * Experimental: Numeric @id support state. Disabled by default.
     *
     * @return <code>true</code> if numeric @id support is enabled
     */
    public boolean useNumericId() {
        return useNumericId;
    }

//    public Cache<String, JsonValue> getContextCache() {
//        return contextCache;
//    }
//
//    public void setContextCache(Cache<String, JsonValue> contextCache) {
//        this.contextCache = contextCache;
//    }
//
    public Cache<String, Document> getDocumentCache() {
        return documentCache;
    }

    public Options setDocumentCache(Cache<String, Document> documentCache) {
        this.documentCache = documentCache;
        return this;
    }

    public boolean isRdfStar() {
        return rdfStar;
    }

    /**
     * Experimental: Enables JSON-LD-STAR extension. Only expansion is supported.
     * Disabled by default.
     *
     * @see <a href="https://json-ld.github.io/json-ld-star">JSON-LD-STAR Draft</a>
     *
     */
    public Options rdfStar(boolean rdfStar) {
        this.rdfStar = rdfStar;
        return this;
    }

    public UriValidationPolicy uriValidation() {
        return uriValidation;
    }

    /**
     * Sets the URI validation policy. By default, the policy is set to
     * {@code Full}.
     * 
     * <p>
     * Note: Changing this policy may produce unexpected or non-compliant results,
     * depending on the specified validation rules.
     * </p>
     * 
     * @param uriValidation the desired URI validation policy
     * 
     * @since 1.5.0
     */
    public Options uriValidation(UriValidationPolicy uriValidation) {
        this.uriValidation = uriValidation;
        return this;
    }

    /**
     * A processing timeout. An exception is thrown when a processing time exceeds
     * the duration, if set. There is no currency that processing gets terminated
     * immediately, but eventually.
     * 
     * Please note, the timeout does not include time consumed by
     * {@link DocumentLoader}.
     * 
     * @return a duration after which a processing is prematurely terminated.
     */
    public Duration timeout() {
        return timeout;
    }

    /**
     * Set a pressing timeout. A processing is eventually terminated after the
     * specified duration. Set <code>null</code> for no timeout.
     * 
     * Please note, the timeout does not include time consumed by
     * {@link DocumentLoader}.
     * 
     * @param timeout to limit processing time
     */
    public Options timeout(Duration timeout) {
        this.timeout = timeout;
        return this;
    }

    /**
     * A processing policy on how proceed with an undefined term during expansion.
     * 
     * @return the processing policy, never <code>null</code>
     */
    public ProcessingPolicy undefinedTermsPolicy() {
        return undefinedTerms;
    }

    /**
     * Set processing policy on how proceed with an undefined term during expansion.
     * Ignore by default.
     * 
     * @param undefinedTerms the processing policy, never <code>null</code>
     * 
     */
    public Options undefinedTermsPolicy(ProcessingPolicy undefinedTerms) {
        this.undefinedTerms = undefinedTerms;
        return this;
    }
}