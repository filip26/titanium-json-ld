package com.apicatalog.jsonld;

import java.util.Map;

/**
 * The {@link JsonLdRecord} is the definition of a map used 
 * to contain arbitrary map entries which are the result of 
 * parsing a <a href="https://tools.ietf.org/html/rfc8259#section-4">JSON Object</a>.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#webidl-43687581">JsonLdRecord IDL</a>
 *
 */
public interface JsonLdRecord {

	public static JsonLdRecord of(Map<String, Object> map) {
		return null;
	}
}
