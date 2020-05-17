package com.apicatalog.jsonld;

import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.FileDocumentLoader;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;

/**
 * The {@link JsonLdOptions} type is used to pass various options to the {@link JsonLdProcessor} methods.
 * 
 * @see <a href=https://www.w3.org/TR/json-ld11-api/#the-jsonldoptions-type">The JsonLdOptions Specification.</a>
 *  
 */
public final class JsonLdOptions {
	
	public static final JsonLdOptions DEFAULT = new JsonLdOptions();
	
	private URI baseUri;
	private boolean compactArrays;
	private boolean compactToRelative;
	private LoadDocumentCallback documentLoader;
	//private final (JsonLdRecord? or USVString) expandContext = null;
	private boolean extractAllScripts;
	private boolean frameExpansion;
	private boolean ordered;
	private Version processingMode;
	private boolean produceGeneralizedRdf;
	private String rdfDirection;
	private boolean useNativeTypes;
	private boolean useRdfType;
	
	protected JsonLdOptions() {
		this.baseUri = null;
		this.compactArrays = true;
		this.compactToRelative = true;
		this.documentLoader = new FileDocumentLoader();
		this.extractAllScripts = false;
		this.frameExpansion = false;
		this.ordered = false;
		this.processingMode = Version.V1_1;
		this.produceGeneralizedRdf = true;
		this.rdfDirection = null;
		this.useNativeTypes = false;
		this.useRdfType = false;
	}
	
	protected JsonLdOptions(JsonLdOptions options) {
		this.baseUri = options.baseUri;
		this.compactArrays = options.compactArrays;
		this.compactToRelative = options.compactToRelative;
		this.documentLoader = options.documentLoader;
		this.extractAllScripts = options.extractAllScripts;
		this.frameExpansion = options.frameExpansion;
		this.ordered = options.ordered;
		this.processingMode = options.processingMode;
		this.produceGeneralizedRdf = options.produceGeneralizedRdf;
		this.rdfDirection = options.rdfDirection;
		this.useNativeTypes = options.useNativeTypes;
		this.useRdfType = options.useRdfType;		
	}
	
	/**
	 * The base IRI to use when expanding or 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a>
	 * the document. If set, this overrides the input document's IRI.
	 * 
	 * @return
	 */
	public URI getBaseURI() {
		return baseUri;
	}
	
	/**
	 * If set to true, the {@link JsonLdProcessor} replaces arrays with just one element with 
	 * that element during 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compaction</a>. 
	 * If set to false, all arrays will remain arrays even 
	 * if they have just one element.
	 * 
	 * @return
	 */
	public boolean isCompactArrays() {
		return compactArrays;
	}
	
	/**
	 * Determines if IRIs are compacted relative to the {@link #getBaseURI()} 
	 * option or document location when <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a>.
	 * 
	 * @return
	 */
	public boolean isCompactToRelative() {
		return compactToRelative;
	}
	
	/**
	 * The callback of the loader to be used to retrieve remote documents 
	 * and contexts, implementing the {@link LoadDocumentCallback}. 
	 * If specified, it is used to retrieve remote documents and contexts; 
	 * otherwise, if not specified, the processor's built-in loader is used.
	 * 
	 * @return
	 */
	public Optional<LoadDocumentCallback> getDocumentLoader() {
		return Optional.ofNullable(documentLoader);
	}
	
	/**
	 * If set to <code>true</code>, when extracting 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-json-ld-script-element">JSON-LD script elements</a> 
	 * from HTML, unless a specific 
	 * <a href="https://tools.ietf.org/html/rfc3986#section-3.5">fragment identifier</a>
	 * is targeted, extracts all encountered 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-json-ld-script-element">JSON-LD script elements</a>
	 * using an array form, if necessary.
	 * 
	 * @return
	 */
	public boolean isExtractAllScripts() {
		return extractAllScripts;
	}
	
	/**
	 * Enables special frame processing rules for the 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion Algorithm</a>.
	 * <p>
     * Enables special rules for the 
     * <a href="https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm">Serialize RDF as JSON-LD Algorithm</a> 
     * to use JSON-LD native types as values, where possible.
     * </p>
     * 
	 * @return
	 */
	public boolean isFrameExpansion() {
		return frameExpansion;
	}
	
	/**
	 * If set to <code>true</code>, certain algorithm processing steps where indicated are ordered lexicographically.
	 * If <code>false</code>, order is not considered in processing.
	 * 
	 * @return
	 */
	public boolean isOrdered() {
		return ordered;
	}
	
	public Version getProcessingMode() {
		return processingMode;
	}
	
	public boolean isProduceGeneralizedRdf() {
		return produceGeneralizedRdf;
	}
	
	public Optional<String> getRdfDirection() {
		return Optional.ofNullable(rdfDirection);
	}
	
	public boolean isUseNativeTypes() {
		return useNativeTypes;
	}
	
	public boolean isUseRdfType() {
		return useRdfType;
	}
	
	protected void setBaseUri(URI baseUri) {
		this.baseUri = baseUri;
	}
	
	protected void setCompactArrays(boolean compactArrays) {
		this.compactArrays = compactArrays;
	}
	
	protected void setCompactToRelative(boolean compactToRelative) {
		this.compactToRelative = compactToRelative;
	}
	
	protected void setDocumentLoader(LoadDocumentCallback documentLoader) {
		this.documentLoader = documentLoader;
	}
	
	protected void setExtractAllScripts(boolean extractAllScripts) {
		this.extractAllScripts = extractAllScripts;
	}
	
	protected void setFrameExpansion(boolean frameExpansion) {
		this.frameExpansion = frameExpansion;
	}
	
	protected void setOrdered(boolean ordered) {
		this.ordered = ordered;
	}
	
	protected void setProcessingMode(Version processingMode) {
		this.processingMode = processingMode;
	}
	
	protected void setProduceGeneralizedRdf(boolean produceGeneralizedRdf) {
		this.produceGeneralizedRdf = produceGeneralizedRdf;
	}
	
	protected void setRdfDirection(String rdfDirection) {
		this.rdfDirection = rdfDirection;
	}
	
	protected void setUseNativeTypes(boolean useNativeTypes) {
		this.useNativeTypes = useNativeTypes;
	}
	
	protected void setUseRdfType(boolean useRdfType) {
		this.useRdfType = useRdfType;
	}
	
}
