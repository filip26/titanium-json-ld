package com.apicatalog.jsonld;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.rdf.RdfDataset;

/**
 * The {@link JsonLdProcessor} interface is the high-level programming 
 * structure that developers use to access the JSON-LD transformation methods.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface">JsonLdProcessor Specification</a>
 *
 */
public interface JsonLdProcessor {

	JsonObject compact(JsonLdInput input) throws JsonLdError;
	
	JsonObject compact(JsonLdInput input, JsonLdContext context) throws JsonLdError;
	
	JsonObject compact(JsonLdInput input, JsonLdOptions options) throws JsonLdError;
	
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
	JsonObject compact(JsonLdInput input, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

	JsonValue expand(JsonLdInput input) throws JsonLdError;
	
	JsonValue expand(JsonLdInput input, JsonLdOptions options) throws JsonLdError;
	
	JsonObject flatten(JsonLdInput input);
	
	JsonObject flatten(JsonLdInput input, JsonLdContext context);
	
	JsonObject flatten(JsonLdInput input, JsonLdOptions options);
	
	JsonObject flatten(JsonLdInput input, JsonLdContext context, JsonLdOptions options);
	
	JsonValue fromRdf(RdfDataset input);
	
	JsonValue fromRdf(RdfDataset input, JsonLdOptions options);
	
	RdfDataset toRdf(JsonLdInput input);
	
	RdfDataset toRdf(JsonLdInput input, JsonLdOptions options);
	
}
