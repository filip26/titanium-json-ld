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
 * Configuration options used by the JSON-LD processor.
 *
 * <p>
 * These options influence expansion, compaction, flattening, framing, and
 * conversion to and from RDF.
 * </p>
 *
 * <p>
 * Instances can be configured using fluent setter style:
 * </p>
 *
 * <pre>{@code
 * Options options = Options.newOptions()
 *         .base(URI.create("https://example.com/"))
 *         .compactArrays(true)
 *         .ordered(false);
 * }</pre>
 *
 * <p>
 * Use {@link #newOptions()} to create a new options instance with default
 * settings, {@link #with(DocumentLoader)} to initialize with a custom document
 * loader, or {@link #copyOf(Options)} to obtain a copy of an existing options
 * object.
 * </p>
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#the-jsonldoptions-type">
 *      JSON-LD 1.1 API - The JsonLdOptions Type</a>
 */
public final class Options {

    /**
     * RDF direction handling strategies.
     */
    public enum RdfDirection {
        /** Use internationalization datatype representation. */
        I18N_DATATYPE,
        /** Use compound literal representation. */
        COMPOUND_LITERAL
    }

    /**
     * Defines how the processor handles certain processing conditions, such as
     * undefined terms.
     */
    public enum ProcessingPolicy {
        /** Ignore the condition (default behavior). */
        Ignore,
        /** Stop processing and throw an error. */
        Fail,
        /** Log a warning but continue processing. */
        Warn
    }

    /** Default settings. */
    public static final boolean DEFAULT_RDF_STAR = false;
    public static final boolean DEFAULT_NUMERIC_ID = false;
    public static final boolean DEFAULT_INLINE_CONTEXTS = true;
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

    // allows numeric @id
    private boolean useNumericId;

    // allows inline contexts
    private boolean useInlineContexts;

    // context cache
//    private Cache<String, JsonValue> contextCache;

    // document cache
    private Cache<String, Document> documentCache;

    private UriValidationPolicy uriValidation;

    private Duration timeout;

    // a policy on how proceed with undefined terms during expansion
    private ProcessingPolicy undefinedTerms;
    
    private ProcessingPolicy droppedNodes;

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
        this.useInlineContexts = DEFAULT_INLINE_CONTEXTS;
//        this.contextCache = new LruCache<>(256);
        this.documentCache = null;
        this.uriValidation = DEFAULT_URI_VALIDATION;
        this.timeout = null;
        this.undefinedTerms = ProcessingPolicy.Ignore;
        this.droppedNodes = ProcessingPolicy.Ignore;
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
        this.useInlineContexts = options.useInlineContexts;
//        this.contextCache = options.contextCache;
        this.documentCache = options.documentCache;
        this.uriValidation = options.uriValidation;
        this.timeout = options.timeout;
        this.undefinedTerms = options.undefinedTerms;
        this.droppedNodes = options.droppedNodes;
    }

    /**
     * Returns a new options object with default settings.
     *
     * @return a new options object
     */
    public static final Options newOptions() {
        return new Options();
    }

    /**
     * Returns a new options object initialized with the provided document loader.
     *
     * @param loader the loader to use for retrieving remote documents and contexts.
     * @return a new options object with the given loader.
     */
    public static final Options with(DocumentLoader loader) {
        return new Options().loader(loader);
    }

    /**
     * Copies the given options to create a new, distinct options object with the
     * same settings.
     *
     * @param options the options to copy
     * @return a new options object with the same values.
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
     * identifier</a> is targeted, extracts all encountered JSON-LD script elements
     * using an array form, if necessary.
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

    /**
     * Specifies which version of the JSON-LD specification (e.g., 1.0 or 1.1) the
     * processor adheres to for all subsequent operations.
     *
     * @return the processing mode.
     */
    public Version mode() {
        return processingMode;
    }

    /**
     * Indicates if the processor is configured to convert the data into generalized
     * RDF, which permits subjects or objects that are not strict IRIs or Literals.
     *
     * @return <code>true</code> if generalized RDF is enabled.
     */
    public boolean isProduceGeneralizedRdf() {
        return generalizedRdf;
    }

    /**
     * Returns the configured RDF direction handling strategy.
     *
     * @return the RDF direction strategy or {@code null} if not set.
     */
    public RdfDirection rdfDirection() {
        return rdfDirection;
    }

    /**
     * Indicates whether native JSON types (such as numbers and booleans) should be
     * used when converting <strong>from RDF to JSON-LD</strong>, instead of using
     * typed string values (e.g., {@code "xsd:boolean"}).
     *
     * @return {@code true} if native JSON types are used when converting from RDF;
     *         otherwise {@code false}.
     */
    public boolean useNativeTypes() {
        return useNativeTypes;
    }

    /**
     * Indicates whether the JSON-LD processor uses the RDF type (i.e.,
     * {@code rdf:type}) instead of the JSON-LD keyword {@code @type}.
     *
     * @return {@code true} if RDF type is used; otherwise {@code false}.
     */
    public boolean useRdfType() {
        return useRdfType;
    }

    /**
     * Returns the {@link RdfJsonLiteralWriter} used when converting {@code @json}
     * type <strong>to RDF</strong> to serialize JSON as {@code rdf:JSON}.
     *
     * @return the configured {@link RdfJsonLiteralWriter}, never {@code null}.
     */
    public RdfJsonLiteralWriter rdfJsonLiteralWriter() {
        return rdfJsonLiteralWriter;
    }

    /**
     * Returns the expand context used to initialize the active context when
     * expanding a document.
     *
     * @return the expand context, or {@code null} if none is set.
     */
    public Document expandContext() {
        return expandContext;
    }

    /**
     * Sets the base IRI to use when expanding or compacting a document.
     *
     * @param base the base IRI as a {@link URI}, or {@code null} to clear it
     * @return this {@link Options} instance, for method chaining
     */
    public Options base(URI base) {
        this.base = base;
        return this;
    }

    /**
     * Sets the base IRI to use when expanding or compacting a document.
     *
     * <p>
     * This is a convenience method equivalent to {@link #base(URI)}, using
     * {@link URI#create(String)} to construct the URI.
     * </p>
     *
     * @param base the base IRI as a string
     * @return this {@link Options} instance, for method chaining
     * @throws IllegalArgumentException if the given string violates {@link URI}
     *                                  syntax rules
     */
    public Options base(String base) {
        this.base = URI.create(base);
        return this;
    }

    /**
     * Enables or disables array compaction during JSON-LD compaction.
     *
     * <p>
     * If {@code true}, arrays with a single element are replaced by that element.
     * If {@code false}, all arrays are preserved.
     * </p>
     *
     * @param compactArrays {@code true} to enable array compaction; {@code false}
     *                      otherwise
     * @return this {@link Options} instance, for method chaining
     */
    public Options compactArrays(boolean compactArrays) {
        this.compactArrays = compactArrays;
        return this;
    }

    /**
     * Sets whether IRIs are compacted relative to the base IRI or document location
     * during compaction.
     *
     * @param compactToRelative {@code true} to enable relative compaction;
     *                          {@code false} to disable it
     * @return this {@link Options} instance, for method chaining
     */
    public Options compactToRelative(boolean compactToRelative) {
        this.compactToRelative = compactToRelative;
        return this;
    }

    /**
     * Sets the {@link DocumentLoader} to be used for retrieving remote documents
     * and contexts.
     *
     * @param loader the loader to use, or {@code null} to use the default loader
     * @return this {@link Options} instance, for method chaining
     */
    public Options loader(DocumentLoader loader) {
        this.loader = loader;
        return this;
    }

    /**
     * Enables or disables extraction of all JSON-LD script elements from HTML
     * input.
     *
     * @param extractAllScripts {@code true} to extract all JSON-LD script elements;
     *                          {@code false} otherwise
     * @return this {@link Options} instance, for method chaining
     */
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

    /**
     * Sets the JSON-LD processing mode (e.g., 1.0 or 1.1) that determines the
     * algorithm behavior.
     *
     * @param mode the {@link Version} to apply
     * @return this {@link Options} instance, for method chaining
     */
    public Options mode(Version mode) {
        this.processingMode = mode;
        return this;
    }

    /**
     * Sets whether the processor produces generalized RDF, which allows subjects or
     * objects that are not IRIs or literals (for example, blank nodes as
     * predicates).
     * 
     * <p>
     * When set to {@code false}, output RDF will conform strictly to the RDF 1.1
     * specification.
     * </p>
     *
     * @param produceGeneralizedRdf {@code true} to enable generalized RDF
     * @return this {@link Options} instance, for method chaining
     */
    public Options generalizedRdf(boolean produceGeneralizedRdf) {
        this.generalizedRdf = produceGeneralizedRdf;
        return this;
    }

    /**
     * Sets the RDF direction handling strategy used when converting to or from RDF
     * literals that include language direction information.
     *
     * @param rdfDirection the RDF direction strategy to apply
     * @return this {@link Options} instance, for method chaining
     */
    public Options rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }

    /**
     * If set to {@code true}, the JSON-LD processor will use native JSON types
     * (such as numbers and booleans) when converting <strong>from RDF to
     * JSON-LD</strong>, instead of using typed string values (e.g.,
     * {@code "xsd:boolean"} or {@code "xsd:integer"}).
     *
     * @param useNativeTypes whether to use native JSON types when converting from
     *                       RDF
     * @return this {@link Options} instance, for method chaining
     */
    public Options useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }

    /**
     * If set to {@code true}, the JSON-LD processor will use the JSON-LD keyword
     * {@code @type} instead of generating {@code rdf:type} statements when
     * converting <strong>from RDF to JSON-LD</strong>.
     *
     * @param useRdfType whether to use {@code rdf:type} instead of {@code @type}
     *                   when converting from RDF
     * @return this {@link Options} instance, for method chaining
     */
    public Options useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }

    /**
     * Sets the {@link RdfJsonLiteralWriter} used for serializing {@code @json} type
     * when converting to {@code rdf:JSON}.
     *
     * @param jsonWriter the JSON literal writer implementation
     * @return this {@link Options} instance, for method chaining
     */
    public Options rdfJsonLiteralWriter(RdfJsonLiteralWriter jsonWriter) {
        this.rdfJsonLiteralWriter = jsonWriter;
        return this;
    }

    /**
     * Sets the expand context by providing an URL representing a context.
     *
     * <p>
     * If the provided URL is {@code null}, the expand context is cleared.
     * </p>
     *
     * @param uri the URL represeting location of the context to load, or
     *            {@code null} to clear
     * @return this {@link Options} instance, for method chaining
     */
    public Options expandContext(final String uri) {

        if (uri == null) {
            this.expandContext = null;
            return this;
        }
        this.expandContext = Document.of(
                new TreeIO(Set.of(uri), NativeAdapter.instance()));
        return this;
    }

    /**
     * Sets the expand context by providing its location as a {@link URI}.
     *
     * <p>
     * If the provided URI is {@code null}, the expand context is cleared.
     * </p>
     *
     * @param uri the URI of the expand context, or {@code null} to clear it
     * @return this {@link Options} instance, for method chaining
     */
    public Options expandContext(URI uri) {

        if (uri == null) {
            this.expandContext = null;
            return this;
        }

        return expandContext(uri.toString());
    }

    /**
     * Sets the expand context using a {@link TreeIO} node.
     *
     * <p>
     * If {@code null} is provided, the expand context is cleared.
     * </p>
     *
     * @param node the tree node representing the context, or {@code null} to clear
     *             it
     * @return this {@link Options} instance, for method chaining
     */
    public Options expandContext(TreeIO node) {

        if (node == null) {
            this.expandContext = null;
            return this;
        }
        this.expandContext = Document.of(node);
        return this;
    }

    /**
     * Sets the expand context directly as a {@link Document}.
     *
     * @param context the document representing the context used when expanding, or
     *                {@code null} to clear it
     * @return this {@link Options} instance, for method chaining
     */
    public Options expandContext(Document context) {
        this.expandContext = context;
        return this;
    }

    // Framing

    /**
     * Returns the current {@link Embed} mode used during framing.
     *
     * @return the embed mode.
     */
    public Embed embed() {
        return embed;
    }

    /**
     * Sets the {@link Embed} mode used during framing.
     *
     * @param embed the embed mode to use
     * @return this {@link Options} instance, for method chaining
     */
    public Options embed(Embed embed) {
        this.embed = embed;
        return this;
    }

    /**
     * Indicates whether framing should include only explicitly defined properties.
     *
     * @return {@code true} if only explicitly defined properties are included
     *         during framing
     */
    public boolean isExplicit() {
        return explicit;
    }

    /**
     * Sets whether framing should include only explicitly defined properties.
     *
     * @param explicit {@code true} to include only explicitly defined properties
     * @return this {@link Options} instance, for method chaining
     */
    public Options explicit(boolean explicit) {
        this.explicit = explicit;
        return this;
    }

    /**
     * Indicates whether default values are omitted during framing output.
     *
     * @return {@code true} if default values are omitted
     */
    public boolean isOmitDefault() {
        return omitDefault;
    }

    /**
     * Sets whether framing should omit default values from the output.
     *
     * @param omitDefault {@code true} to omit default values
     * @return this {@link Options} instance, for method chaining
     */
    public Options omitDefault(boolean omitDefault) {
        this.omitDefault = omitDefault;
        return this;
    }

    /**
     * Indicates whether framing should omit the top-level graph container when only
     * a single node exists.
     *
     * @return {@code true} to omit the graph; {@code false} to include it; or
     *         {@code null} if the default behavior is used
     */
    public Boolean isOmitGraph() {
        return omitGraph;
    }

    /**
     * Sets whether framing output should omit the top-level graph container when
     * only a single node exists.
     *
     * @param omitGraph {@code true} to omit the graph; {@code false} to include it;
     *                  or {@code null} to use the default behavior
     * @return this {@link Options} instance, for method chaining
     */
    public Options omitGraph(Boolean omitGraph) {
        this.omitGraph = omitGraph;
        return this;
    }

    /**
     * Indicates whether all properties listed in a frame must be present in the
     * matched nodes during framing.
     *
     * <p>
     * If {@code true}, only nodes that include every property defined in the frame
     * will match. If {@code false}, partial matches are allowed.
     * </p>
     *
     * @return {@code true} if all frame properties are required; otherwise
     *         {@code false}
     */
    public boolean isRequiredAll() {
        return requiredAll;
    }

    /**
     * Sets whether framing requires all properties listed in the frame to appear in
     * the matched nodes.
     *
     * @param requiredAll {@code true} if all properties must be present
     * @return this {@link Options} instance, for method chaining
     */
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

    /**
     * Returns the cache used for storing retrieved remote documents.
     *
     * @return the document cache, or {@code null} if none is configured
     */
    public Cache<String, Document> getDocumentCache() {
        return documentCache;
    }

    /**
     * Sets the cache used to store retrieved remote documents.
     *
     * @param documentCache the cache to use, or {@code null} to disable caching
     * @return this {@link Options} instance, for method chaining
     */
    public Options setDocumentCache(Cache<String, Document> documentCache) {
        this.documentCache = documentCache;
        return this;
    }

    /**
     * Indicates whether the JSON-LD-star is enabled.
     *
     * @return {@code true} if JSON-LD-star is enabled; otherwise {@code false}.
     */
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

    /**
     * Returns the URI validation policy currently in use.
     *
     * <p>
     * The default policy is {@link UriValidationPolicy#Full}.
     * </p>
     *
     * @return the current URI validation policy, never {@code null}
     *
     * @since 1.5.0
     */
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
     * Please note, the timeout does not affect time consumed by
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
     * Please note, the timeout does not affect time consumed by
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
    public ProcessingPolicy undefinedTerms() {
        return undefinedTerms;
    }

    /**
     * Set processing policy on how proceed with an undefined term during expansion.
     * Ignore by default.
     * 
     * @param undefinedTerms the processing policy, never <code>null</code>
     * 
     */
    public Options undefinedTerms(ProcessingPolicy undefinedTerms) {
        this.undefinedTerms = undefinedTerms;
        return this;
    }

    public boolean useInlineContexts() {
        return useInlineContexts;
    }

    public Options useInlineContexts(boolean useInlineContext) {
        this.useInlineContexts = useInlineContext;
        return this;
    }
    
    public ProcessingPolicy droppedNodes() {
        return droppedNodes;
    }
    
    public Options droppedNodes(ProcessingPolicy droppedNodes) {
        this.droppedNodes = droppedNodes;
        return this;
    }
}