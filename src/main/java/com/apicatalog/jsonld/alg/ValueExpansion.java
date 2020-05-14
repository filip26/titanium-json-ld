package com.apicatalog.jsonld.alg;

import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLdContext;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-expansion">Value Expansion Algorithm</a>
 *
 */
public class ValueExpansion {

	public static final JsonValue expand(JsonLdContext context, JsonValue element, String property) {
		
		return element;
	}
	
}
