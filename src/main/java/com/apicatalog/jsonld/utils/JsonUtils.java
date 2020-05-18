package com.apicatalog.jsonld.utils;

import javax.json.Json;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;

public final class JsonUtils {

	JsonUtils() {
	}
	
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
	
	public static final boolean isScalar(final JsonValue value) {
		return value != null
				&& (ValueType.STRING.equals(value.getValueType())
				|| ValueType.NUMBER.equals(value.getValueType())
				|| ValueType.TRUE.equals(value.getValueType())
				|| ValueType.FALSE.equals(value.getValueType())
				);
	}
	
	public static final boolean isNotScalar(final JsonValue value) {
		return !isScalar(value);
	}
	
	
	public static final boolean isNull(final JsonValue value) {
		return value == null || ValueType.NULL.equals(value.getValueType());
	}
	
	public static final boolean isNotNull(final JsonValue value) {
		return !isNull(value);
	}
	
	public static boolean isString(JsonValue value) {
		return value != null && ValueType.STRING.equals(value.getValueType());
	}


	public static boolean isNotString(JsonValue value) {
		return value == null || !ValueType.STRING.equals(value.getValueType());
	}

	public static boolean isNotArray(JsonValue value) {
		return value == null || !ValueType.ARRAY.equals(value.getValueType());
	}
	
}