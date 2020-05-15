package com.apicatalog.jsonld.alg;

import javax.json.JsonValue;

import com.apicatalog.jsonld.impl.ActiveContext;
import com.apicatalog.jsonld.impl.Keywords;
/**
 * 
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#algorithm-4">IRI Expansion</a>
 *
 */
public class UriExpansion {

	public static final String expand(
				ActiveContext activeContext,
				String value
			) {
		
		if (value == null || Keywords.contains(value)) {
			return value;
		}
		
		return value;
		
	}
	
	
}
