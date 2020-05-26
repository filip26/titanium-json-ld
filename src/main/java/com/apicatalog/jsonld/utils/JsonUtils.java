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

        if (text == null) {
            return value == null;
        }
        
        if (value == null) {
            return false;
        }

        if (JsonUtils.isString(value)) {
            return text.equals(((JsonString) value).getString());
        }

        if (JsonUtils.isArray(value)) {
            return value.asJsonArray().contains(Json.createValue(text));
        }
        if (JsonUtils.isObject(value)) {
            return value.asJsonObject().containsKey(text);
        }
        
        return false;
    }

    public static final boolean isScalar(final JsonValue value) {
        return value != null 
                    && !ValueType.ARRAY.equals(value.getValueType())
                    && !ValueType.OBJECT.equals(value.getValueType());
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
                        && !ValueType.FALSE.equals(value.getValueType()));
    }

    public static boolean isTrue(JsonValue value) {
        return value != null && ValueType.TRUE.equals(value.getValueType());
    }

    public static boolean isFalse(JsonValue value) {
        return value == null || ValueType.FALSE.equals(value.getValueType());
    }

    public static boolean isEmptyObject(JsonValue value) {
        return isObject(value) && value.asJsonObject().isEmpty();
    }

    public static boolean isEmptyArray(JsonValue value) {
        return isArray(value) && value.asJsonArray().isEmpty();
    }

    public static JsonObject toJsonObject(Map<String, JsonValue> map) {
        final JsonObjectBuilder builder = Json.createObjectBuilder();

        map.entrySet().stream().forEach(e -> builder.add(e.getKey(), e.getValue()));

        return builder.build();
    }

    public static JsonObject merge(JsonObject target, JsonObject source) {
        Map<String, JsonValue> targetMap = (new LinkedHashMap<>(target));

        source.forEach(targetMap::put);

        return toJsonObject(targetMap);
    }

    public static JsonArray toJsonArray(JsonValue value) {
        return JsonUtils.isArray(value) ? value.asJsonArray() : Json.createArrayBuilder().add(value).build();
    }

    public static boolean isBlankString(JsonValue value) {
        return isString(value) && ((JsonString) value).getString().isBlank();
    }

    public static JsonValue toJsonValue(String value) {
        return value != null && !value.isBlank() 
                    ? Json.createValue(value)
                    : JsonValue.NULL
                    ;
    }    
    
}