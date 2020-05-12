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

	JsonLdRecord compact(JsonLdInput input);
	
	JsonLdRecord compact(JsonLdInput input, JsonLdContext context);
	
	JsonLdRecord compact(JsonLdInput input, JsonLdOptions options);
	
	JsonLdRecord compact(JsonLdInput input, JsonLdContext context, JsonLdOptions options);

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
