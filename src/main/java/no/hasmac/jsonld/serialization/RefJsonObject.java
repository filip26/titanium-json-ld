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
package no.hasmac.jsonld.serialization;

import java.util.AbstractMap;
import java.util.Set;

import no.hasmac.jsonld.json.JsonProvider;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

final class RefJsonObject extends AbstractMap<String, JsonValue> implements JsonObject {

    private JsonObject jsonObject;

    RefJsonObject(JsonObject jsonObject) {
        this.jsonObject = jsonObject;
    }

    @Override
    public JsonArray getJsonArray(String name) {
        return jsonObject.getJsonArray(name);
    }

    @Override
    public JsonObject getJsonObject(String name) {
        return jsonObject.getJsonObject(name);
    }

    @Override
    public JsonNumber getJsonNumber(String name) {
        return jsonObject.getJsonNumber(name);
    }

    @Override
    public JsonString getJsonString(String name) {
        return jsonObject.getJsonString(name);
    }

    @Override
    public String getString(String name) {
        return jsonObject.getString(name);
    }

    @Override
    public String getString(String name, String defaultValue) {
        return jsonObject.getString(name, defaultValue);
    }

    @Override
    public int getInt(String name) {
        return jsonObject.getInt(name);
    }

    @Override
    public int getInt(String name, int defaultValue) {
        return jsonObject.getInt(name, defaultValue);
    }

    @Override
    public boolean getBoolean(String name) {
        return jsonObject.getBoolean(name);
    }

    @Override
    public boolean getBoolean(String name, boolean defaultValue) {
        return jsonObject.getBoolean(name, defaultValue);
    }

    @Override
    public boolean isNull(String name) {
        return jsonObject.isNull(name);
    }

    @Override
    public ValueType getValueType() {
        return jsonObject.getValueType();
    }

    @Override
    public Set<Entry<String, JsonValue>> entrySet() {
        return jsonObject.entrySet();
    }

    @Override
    public String toString() {
        return jsonObject.toString();
    }

    @Override
    public JsonObject asJsonObject() {
        return this;
    }

    @Override
    public int size() {
        return jsonObject.size();
    }

    @Override
    public JsonValue get(Object key) {
        return jsonObject.get(key);
    }

    @Override
    public boolean containsKey(Object key) {
        return jsonObject.containsKey(key);
    }

    @Override
    public int hashCode() {
        return jsonObject.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        return jsonObject.equals(o);
    }

    @Override
    public JsonValue put(String key, JsonValue value) {
        JsonValue previous = jsonObject.get(key);

        jsonObject = JsonProvider.instance().createObjectBuilder(jsonObject).add(key, value).build();

        return previous;
    }

    @Override
    public JsonValue remove(Object key) {

        JsonValue previous = jsonObject.get(key);

        jsonObject = JsonProvider.instance().createObjectBuilder(jsonObject).remove(key.toString()).build();

        return previous;
    }
}
