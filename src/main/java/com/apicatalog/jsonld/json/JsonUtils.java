/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.json;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
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
        return value != null && ValueType.FALSE.equals(value.getValueType());
    }

    public static boolean isEmptyObject(JsonValue value) {
        return isObject(value) && value.asJsonObject().isEmpty();
    }

    public static boolean isEmptyArray(JsonValue value) {
        return isArray(value) && value.asJsonArray().isEmpty();
    }

    public static JsonObject toJsonObject(Map<String, JsonValue> map) {
        final JsonObjectBuilder builder = Json.createObjectBuilder();

        map.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));

        return builder.build();
    }

    public static JsonArray toJsonArray(Collection<JsonValue> collection) {
        
        final JsonArrayBuilder builder = Json.createArrayBuilder();

        collection.forEach(builder::add);

        return builder.build();
    }

    public static JsonObject merge(JsonObject target, JsonObject source) {
        Map<String, JsonValue> targetMap = (new LinkedHashMap<>(target));

        source.forEach(targetMap::put);

        return toJsonObject(targetMap);
    }

    public static JsonArray toJsonArray(JsonValue value) {
       return JsonUtils.isArray(value) 
                    ? value.asJsonArray() 
                    : Json.createArrayBuilder().add(value).build()
                    ;
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
    
    public static void addValue(Map<String, JsonValue> object, String key, JsonValue value, boolean asArray) {

        // 1. If as array is true and the value of key in object does not exist or is
        // not an array,
        // set it to a new array containing any original value.
        if (asArray) {

            if (!object.containsKey(key)) {
                object.put(key, Json.createArrayBuilder().build());

            } else {

                JsonValue original = object.get(key);

                if (JsonUtils.isNotArray(original)) {
                    object.put(key, Json.createArrayBuilder().add(original).build());
                }
            }
        }

        // 2. If value is an array, then for each element v in value, use add value
        // recursively to add v to key in entry.
        if (JsonUtils.isArray(value)) {

            for (JsonValue v : value.asJsonArray()) {
                addValue(object, key, v, asArray);
            }

        // 3.
        } else {
            
            // 3.1
            if (!object.containsKey(key)) {                                
                object.put(key, value);

            // 3.2
            } else {

                JsonValue original = object.get(key);

                // 3.2.1
                if (JsonUtils.isNotArray(original)) {
                    object.put(key, Json.createArrayBuilder().add(original).add(value).build());

                // 3.2.2
                } else {
                    object.put(key, Json.createArrayBuilder(original.asJsonArray()).add(value).build());
                }
            }
        }
    }

    public static boolean isNotEmptyArray(JsonValue value) {
        return isNotArray(value) || !value.asJsonArray().isEmpty();
    }
    
    public static boolean isNotEmptyObject(JsonValue value) {
        return isNotObject(value) || !value.asJsonObject().isEmpty();
    }

}