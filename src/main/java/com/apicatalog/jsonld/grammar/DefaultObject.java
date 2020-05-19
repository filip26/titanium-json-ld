package com.apicatalog.jsonld.grammar;

import javax.json.JsonValue;

import com.apicatalog.jsonld.utils.JsonUtils;

public class DefaultObject {

	DefaultObject() {
	}
	
	
	/**
	 * A default object is a map that has a @default key.
	 * 
     * @see <a href="https://www.w3.org/TR/json-ld11/#dfn-default-object">Default Object</a>
     * 
	 * @param value
	 * @return
	 */
	public static final boolean isDefaultObject(JsonValue value) {
		return JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.DEFAULT);		
	}


	public static JsonValue getValue(JsonValue value) {
		return JsonUtils.isObject(value) ? value.asJsonObject().get(Keywords.DEFAULT) : null;
	}
	
}
