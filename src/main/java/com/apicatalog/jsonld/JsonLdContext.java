package com.apicatalog.jsonld;

import java.util.Collection;
import java.util.Map;

import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.impl.LocalContext;

/**
 * The {@link JsonLdContext} interface is used to refer to a value 
 * that may be a {@link JsonLdRecord}, a sequence of {@link JsonLdRecord}, 
 * or a string representing an <code>IRI</code>, which can be dereferenced to retrieve a 
 * valid JSON document.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldcontext">JsonLdContext Specification</a>
 *
 */
public interface JsonLdContext {

	public Collection<JsonLdContext> getContexts();

	public static JsonLdContext of(JsonValue jsonValue) {
		return new LocalContext();
	}
//
//	public static JsonLdContext of(Map<String, Object>...maps) {
//		return null;
//	}
//
//	public static JsonLdContext of(String...iris) {
//		return null;
//	}
}
