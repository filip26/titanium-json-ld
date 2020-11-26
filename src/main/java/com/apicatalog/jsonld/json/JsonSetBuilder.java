package com.apicatalog.jsonld.json;

import java.util.HashSet;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;

public class JsonSetBuilder {

    private final JsonArrayBuilder builder;
    private final Set<JsonValue> set;

    private JsonSetBuilder(final JsonArrayBuilder builder) {
        this.builder = builder;
        this.set = new HashSet<>();
    }

    public static JsonSetBuilder create() {
        return new JsonSetBuilder(Json.createArrayBuilder());
    }

    public JsonArray build() {
        return builder.build();
    }

    public void add(JsonValue value) {
        if (set.contains(value)) {
            return;
        }
        set.add(value);
        builder.add(value);
    }
}
