package com.apicatalog.jsonld.grammar;

import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

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
		return ValueType.OBJECT.equals(value.getValueType()) && value.asJsonObject().containsKey(Keywords.DEFAULT);		
	}
	
}
