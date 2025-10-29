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
package com.apicatalog.jsonld.rdf.in;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.lang.Keywords;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

final class GraphMap {

    static record Reference(
            String graphName,
            String subject,
            String property,
            JsonObject value) {
    }

    // graph, subject, predicate, object
    private final Map<String, Map<String, Map<String, JsonValue>>> index;

    private final Map<String, Map<String, List<Reference>>> usages;

    public GraphMap() {
        this.index = new LinkedHashMap<>();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());

        this.usages = new LinkedHashMap<>();
    }

    public boolean contains(final String graphName, final String subject) {
        return index.containsKey(graphName) && index.get(graphName).containsKey(subject);
    }

    public void set(final String graphName, final String subject, final String property, final JsonValue value) {
        index
                .computeIfAbsent(graphName, e -> new LinkedHashMap<>())
                .computeIfAbsent(subject, e -> new LinkedHashMap<>())
                .put(property, value);
    }

    public Optional<Map<String, JsonValue>> get(final String graphName, final String subject) {

        final Map<String, Map<String, JsonValue>> graphMap = index.get(graphName);

        if (graphMap == null) {
            return Optional.empty();
        }

        return Optional.ofNullable(graphMap.get(subject));
    }

    public Optional<JsonValue> get(final String graphName, final String subject, final String property) {

        final Map<String, Map<String, JsonValue>> graphMap = index.get(graphName);

        if (graphMap == null) {
            return Optional.empty();
        }

        final Map<String, JsonValue> subjectMap = graphMap.get(subject);

        if (subjectMap == null) {
            return Optional.empty();
        }

        return Optional.ofNullable(subjectMap.get(property));
    }

    public Set<String> keys(String graphName) {
        return index.get(graphName).keySet();
    }

    public boolean contains(String graphName) {
        return index.containsKey(graphName);
    }

    public Set<String> keys() {
        return index.keySet();
    }

    public List<Reference> getUsages(String graphName, String subject) {
        return usages.containsKey(graphName) && usages.get(graphName).containsKey(subject)
                ? usages.get(graphName).get(subject)
                : List.of();
    }

    public void addUsage(String graphName, String subject, Reference reference) {
        usages.computeIfAbsent(graphName, e -> new LinkedHashMap<>())
                .computeIfAbsent(subject, e -> new ArrayList<>())
                .add(reference);
    }

    public void remove(String graphName, String subject) {
        index.get(graphName).remove(subject);
    }

    public void clear() {
        this.index.clear();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());
        this.usages.clear();
    }
}