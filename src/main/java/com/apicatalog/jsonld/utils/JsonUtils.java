package com.apicatalog.jsonld.utils;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
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
				&& !ValueType.ARRAY.equals(value.getValueType())
				&& !ValueType.OBJECT.equals(value.getValueType())
				;
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

	public static boolean isArray(JsonValue value) {
		return value != null && ValueType.ARRAY.equals(value.getValueType());
	}
	
	public static JsonValue last(JsonArray array) {
		return array != null ? array.get(array.size() - 1) : null;
	}
	
//	public static String fisrtKeyExpandingTo(final String value, JsonObject object) {
//		return object.entrySet()
//					.stream()
//						.filter(e -> e.getValue().equals(Json.createValue(value)))
//						.map(Map.Entry::getKey)
//						.findFirst()
//						.orElse(null);
//	}
	
	public static boolean isObject(JsonValue value) {
		return value != null && ValueType.OBJECT.equals(value.getValueType());
	}

	public static boolean isNotObject(JsonValue value) {
		return value == null || !ValueType.OBJECT.equals(value.getValueType());
	}

	public static boolean isNumber(JsonValue value) {
		return value != null && ValueType.NUMBER.equals(value.getValueType());
	}

	public static boolean isNotBoolean(JsonValue value) {
		return value == null 
					|| (!ValueType.TRUE.equals(value.getValueType()) 
							&& !ValueType.FALSE.equals(value.getValueType())
							);
	}

	public static boolean isTrue(JsonValue value) {
		return value != null && ValueType.TRUE.equals(value.getValueType());
	}

	public static boolean isEmptyObject(JsonValue value) {
		return isObject(value) && value.asJsonObject().isEmpty();
	}
	
	public static JsonObject toObject(Map<String, JsonValue> map) {
		final JsonObjectBuilder builder = Json.createObjectBuilder();

		map.entrySet()
				.stream()
				.forEach(e -> builder.add(e.getKey(), e.getValue()));
		
		return builder.build();
	}
	
	public static JsonObject merge(JsonObject target, JsonObject source) {
		
		Map<String, JsonValue> targetMap = (new LinkedHashMap<>(target));
				
		source.forEach(targetMap::put);
		
		return toObject(targetMap);
	}
	
	public static JsonArray toArray(JsonValue value) {
		return JsonUtils.isArray(value)
					? value.asJsonArray()
					: Json.createArrayBuilder().add(value).build();
	}

}