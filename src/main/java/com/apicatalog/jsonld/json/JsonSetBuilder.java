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

import java.util.HashSet;
import java.util.Set;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

public final class JsonSetBuilder {

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
