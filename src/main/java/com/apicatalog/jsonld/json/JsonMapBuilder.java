package com.apicatalog.jsonld.json;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;

import com.apicatalog.jsonld.lang.Keywords;

public final class JsonMapBuilder {

    private final Map<String, JsonValue> map;
    
    private JsonMapBuilder(Map<String, JsonValue> map) {
        this.map = map;
    }
    
    public JsonObject build() {
        
        final JsonObjectBuilder builder = Json.createObjectBuilder();

        map.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));
        
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

    public static JsonMapBuilder create() {
        return new JsonMapBuilder(new LinkedHashMap<>());
    }

    public Optional<JsonValue> get(String key) {        
        return Optional.ofNullable(map.get(key));
    }

    public boolean isNotValueObject() {
        return !Keywords.allMatch(map.keySet(), Keywords.TYPE, Keywords.VALUE, Keywords.DIRECTION, Keywords.LANGUAGE, Keywords.INDEX);
    }

    public JsonArray valuesToArray() {
        final JsonArrayBuilder array = Json.createArrayBuilder();

        map.values().forEach(array::add);
        
        return array.build();
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

            final JsonValue original = map.get(key);
            
            // 3.1
            if (original != null) {
                
                if (JsonUtils.isArray(original)) {
                    map.put(key, Json.createArrayBuilder(original.asJsonArray()).add(value).build());
                    
                } else {
                    map.put(key, Json.createArrayBuilder().add(original).add(value).build());
                }

            // 3.2
            } else {
                map.put(key, value);

            }
        }
    }

    public void add(String key, JsonObjectBuilder value) {
        add(key, value.build(), true);
    }

    private void toArray(String key) {

        if (map.containsKey(key)) {
            
            JsonValue original = map.get(key);

            if (JsonUtils.isArray(original)) {
                map.put(key, Json.createArrayBuilder(original.asJsonArray()).build());
                
            } else {
                map.put(key, Json.createArrayBuilder().add(original).build());
            }
            
            return;
        }
        
        map.put(key, JsonValue.EMPTY_JSON_ARRAY);
    }

    public void put(String key, JsonObjectBuilder value) {
        put(key, value.build());
    }

    public void put(String key, JsonMapBuilder value) {
        put(key, value.build());
    }
    
    public Optional<JsonMapBuilder> getMapBuilder(final String key) {
        
        final JsonValue value = map.get(key);
        
        if (value != null) {
            return Optional.of(JsonMapBuilder.create(value.asJsonObject()));
        }
        
        return Optional.empty();
    }
    
//    public Optional<JsonArrayBuilder> getArrayBuilder(final String key) {
//        if (map.containsKey(key)) {
//            JsonValue value = map.get(key);
//            
//            if (value != null) {
//
//                if (JsonUtils.isArray(value)) {
//                    return Optional.of(Json.createArrayBuilder(value.asJsonArray()));
//                }
//                return Optional.of(Json.createArrayBuilder().add(value));
//            }
//            
//        }
//        return Optional.empty();
//    }

}
