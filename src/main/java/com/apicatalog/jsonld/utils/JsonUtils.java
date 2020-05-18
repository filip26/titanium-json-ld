package com.apicatalog.jsonld.utils;

import javax.json.Json;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

public class JsonUtils {

	public static final boolean contains(String text, JsonValue value) {
		
		if (value == null) {
			return false;
		}
		
		if (ValueType.STRING.equals(value.getValueType())) {
			return text.equals(((JsonString)value).getString());
		}
		
		if (ValueType.ARRAY.equals(value.getValueType())) {
			return value.asJsonArray().contains(Json.createValue(text));
		}
		return false;		
	}
	
}
