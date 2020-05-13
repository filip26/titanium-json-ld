package com.apicatalog.jsonld;

import java.util.Collection;

import com.apicatalog.rdf.RdfDataset;

/**
 * The {@link JsonLdProcessor} interface is the high-level programming 
 * structure that developers use to access the JSON-LD transformation methods.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface">JsonLdProcessor Specification</a>
 *
 */
public interface JsonLdProcessor {

	JsonLdRecord compact(JsonLdInput input) throws JsonLdError;
	
	JsonLdRecord compact(JsonLdInput input, JsonLdContext context) throws JsonLdError;
	
	JsonLdRecord compact(JsonLdInput input, JsonLdOptions options) throws JsonLdError;
	
	/**
	 * This algorithm compacts a JSON-LD document, such that the given context is applied. 
	 * This must result in shortening any applicable IRIs to terms or compact IRIs, 
	 * any applicable keywords to keyword aliases, and any applicable JSON-LD values 
	 * expressed in expanded form to simple values such as strings or numbers.
	 * 
	 * @see <a href="https://www.w3.org/TR/json-ld11-api/#compaction-algorithm">Compaction Algorithm</a>
	 * 
	 * @param input
	 * @param context
	 * @param options
	 * @return
	 */
	JsonLdRecord compact(JsonLdInput input, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

	Collection<JsonLdRecord> expand(JsonLdInput input);
	
	Collection<JsonLdRecord> expand(JsonLdInput input, JsonLdOptions options);
	
	JsonLdRecord flatten(JsonLdInput input);
	
	JsonLdRecord flatten(JsonLdInput input, JsonLdContext context);
	
	JsonLdRecord flatten(JsonLdInput input, JsonLdOptions options);
	
	JsonLdRecord flatten(JsonLdInput input, JsonLdContext context, JsonLdOptions options);
	
	Collection<JsonLdRecord> fromRdf(RdfDataset input);
	
	Collection<JsonLdRecord> fromRdf(RdfDataset input, JsonLdOptions options);
	
	RdfDataset toRdf(JsonLdInput input);
	
	RdfDataset toRdf(JsonLdInput input, JsonLdOptions options);
	
}
