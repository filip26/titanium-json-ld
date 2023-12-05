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

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public final class JsonMapBuilder {

    private static final Collection<String> VALUE_KEYWORDS = Arrays.asList(
            Keywords.TYPE, 
            Keywords.VALUE, 
            Keywords.DIRECTION, 
            Keywords.LANGUAGE, 
            Keywords.INDEX, 
            Keywords.ANNOTATION);

    private final Map<String, Object> map;

    private JsonMapBuilder(Map<String, Object> map) {
        this.map = map;
    }

    public JsonObject build() {

        final JsonObjectBuilder builder = JsonProvider.instance().createObjectBuilder();

        for (final Map.Entry<String, Object> entry : map.entrySet()) {

            if (entry.getValue() instanceof JsonValue) {
                builder.add(entry.getKey(), (JsonValue)entry.getValue());

            } else if (entry.getValue() instanceof JsonArrayBuilder) {
                builder.add(entry.getKey(), ((JsonArrayBuilder)entry.getValue()).build());

            } else if (entry.getValue() instanceof JsonMapBuilder) {
                builder.add(entry.getKey(), ((JsonMapBuilder)entry.getValue()).build());

            } else {
                throw new IllegalStateException();
            }
        }

        return builder.build();
    }

    public boolean containsKey(String key) {
        return map.containsKey(key);
    }

    public void put(String key, final JsonValue item) {
        map.put(key, item);
    }

    public int size() {
        return map.size();
    }

    public boolean isEmpty() {
        return map.isEmpty();
    }

    public static JsonMapBuilder create(JsonObject object) {
        return new JsonMapBuilder(new LinkedHashMap<>(object));
    }

    public static JsonMapBuilder create(Map<String, JsonValue> object) {
        return new JsonMapBuilder(new LinkedHashMap<>(object));
    }

    public static JsonMapBuilder create() {
        return new JsonMapBuilder(new LinkedHashMap<>());
    }

    public Optional<JsonValue> get(String key) {

        Object item = map.get(key);

        if (item == null) {
            return Optional.empty();
        }

        if (item instanceof JsonValue) {
            return Optional.of((JsonValue)item);

        } else if (item instanceof JsonArrayBuilder) {
            return Optional.of(((JsonArrayBuilder)item).build());

        } else if (item instanceof JsonMapBuilder) {
            return Optional.of(((JsonMapBuilder)item).build());
        }
        throw new IllegalStateException();
    }
    
    public boolean isNotValueObject() {
        return !VALUE_KEYWORDS.containsAll(map.keySet());
    }

    public JsonArray valuesToArray() {
        final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();

        for (final Object item : map.values()) {

            if (item instanceof JsonValue) {
                array.add((JsonValue)item);

            } else if (item instanceof JsonArrayBuilder) {
                array.add(((JsonArrayBuilder)item).build());

            } else if (item instanceof JsonMapBuilder) {
                array.add(((JsonMapBuilder)item).build());

            } else {
                throw new IllegalStateException();
            }
        }

        return array.build();
    }

    public void add(String key, JsonValue value) {
        add(key, value, true);
    }

    public void add(String key, JsonValue value, boolean asArray) {

        // 1. If as array is true and the value of key in object does not exist or is
        // not an array,
        // set it to a new array containing any original value.
        if (asArray) {
            toArray(key);
        }

        // 2. If value is an array, then for each element v in value, use add value
        // recursively to add v to key in entry.
        if (JsonUtils.isArray(value)) {
            value.asJsonArray().forEach(v -> add(key, v, asArray));

        // 3.
        } else {

            final Object original = map.get(key);

            // 3.1
            if (original != null) {

                if (original instanceof JsonValue) {

                    if (JsonUtils.isArray((JsonValue)original)) {
                        map.put(key, JsonProvider.instance().createArrayBuilder(((JsonValue)original).asJsonArray()).add(value));

                    } else {
                        map.put(key, JsonProvider.instance().createArrayBuilder().add((JsonValue)original).add(value));
                    }

                } else if (original instanceof JsonArrayBuilder) {
                    ((JsonArrayBuilder)original).add(value);

                } else if (original instanceof JsonMapBuilder) {
                    map.put(key, JsonProvider.instance().createArrayBuilder().add(((JsonMapBuilder)original).build()));

                } else {
                    throw new IllegalStateException();
                }

            // 3.2
            } else {
                map.put(key, value);
            }
        }
    }

    public void add(String key, JsonObjectBuilder value) {
        add(key, value.build());
    }

    private void toArray(String key) {

        if (map.containsKey(key)) {

            final Object original = map.get(key);

            if (original instanceof JsonValue) {

                if (JsonUtils.isArray((JsonValue)original)) {
                    map.put(key, JsonProvider.instance().createArrayBuilder(((JsonValue)original).asJsonArray()));

                } else {
                    map.put(key, JsonProvider.instance().createArrayBuilder().add((JsonValue)original));
                }
                return;

            } else if (original instanceof JsonArrayBuilder) {
                return;

            } else if (original instanceof JsonMapBuilder) {
                map.put(key, JsonProvider.instance().createArrayBuilder().add(((JsonMapBuilder)original).build()));
                return;

            }
            throw new IllegalStateException();
        }

        map.put(key, JsonProvider.instance().createArrayBuilder());
    }

    public void put(String key, JsonMapBuilder value) {
        map.put(key, value);
    }

    public JsonMapBuilder getMapBuilder(final String key) {

        final Object value = map.get(key);

        if (value != null) {

            if (value instanceof JsonMapBuilder) {
                return (JsonMapBuilder)value;
            }

            if (value instanceof JsonValue) {
                return JsonMapBuilder.create(((JsonValue)value).asJsonObject());
            }

           throw new IllegalStateException();

        }

        final JsonMapBuilder result = JsonMapBuilder.create();
        map.put(key, result);

        return result;
    }

    public void remove(String key) {
        map.remove(key);
    }

    @Override
    public String toString() {
        return map.toString();
    }
}
