package com.apicatalog.jsonld;

import java.util.Optional;

import com.apicatalog.jsonld.document.LoadDocumentCallback;
import com.apicatalog.jsonld.impl.DefaultDocumentLoader;

/**
 * The {@link JsonLdOptions} type is used to pass various options to the {@link JsonLdProcessor} methods.
 * 
 * @see <a href=https://www.w3.org/TR/json-ld11-api/#the-jsonldoptions-type">The JsonLdOptions Specification.</a>
 *  
 */
public final class JsonLdOptions {

    public static final String JSON_LD_1_0 = "json-ld-1.0";

    public static final String JSON_LD_1_1 = "json-ld-1.1";
	
	public static final JsonLdOptions DEFAULT = new JsonLdOptions();
	
	private final String base;
	private final boolean compactArrays;
	private final boolean compactToRelative;
	private final LoadDocumentCallback documentLoader;
	//private final (JsonLdRecord? or USVString) expandContext = null;
	private final boolean extractAllScripts;
	private final boolean frameExpansion;
	private final boolean ordered;
	private final String processingMode;
	private final boolean produceGeneralizedRdf;
	private final String rdfDirection;
	private final boolean useNativeTypes;
	private final boolean useRdfType;
	
	protected JsonLdOptions() {
		this.base = null;
		this.compactArrays = true;
		this.compactToRelative = true;
		this.documentLoader = new DefaultDocumentLoader();
		this.extractAllScripts = false;
		this.frameExpansion = false;
		this.ordered = false;
		this.processingMode = JSON_LD_1_1;
		this.produceGeneralizedRdf = true;
		this.rdfDirection = null;
		this.useNativeTypes = false;
		this.useRdfType = false;
	}
	
	/**
	 * The base IRI to use when expanding or 
	 * <a href="https://www.w3.org/TR/json-ld11-api/#dfn-compact">compacting</a>
	 * the document. If set, this overrides the input document's IRI.
	 * 
	 * @return
	 */
	public Optional<String> getBase() {
		return Optional.ofNullable(base);
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
	 * Determines if IRIs are compacted relative to the {@link #getBase()} 
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
	
	public String getProcessingMode() {
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
	  
	public class Builder {
		
	}
}
