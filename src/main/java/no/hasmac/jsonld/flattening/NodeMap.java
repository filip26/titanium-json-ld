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
package no.hasmac.jsonld.flattening;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonValue;

public final class NodeMap {

    private final Map<String, Map<String, Map<String, JsonValue>>> index;

    private final BlankNodeIdGenerator generator = new BlankNodeIdGenerator();

    public NodeMap() {
        this.index = new LinkedHashMap<>();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());
    }

    public void set(String graphName, String subject, String property, JsonValue value) {

        if (subject == null) {
            return;
        }

        index
                .computeIfAbsent(graphName, x -> new LinkedHashMap<>())
                .computeIfAbsent(subject, x -> new LinkedHashMap<>())
                .put(property, value);
    }

    public JsonValue get(String graphName, String subject, String property) {

        var indexGraphName = index.get(graphName);
        if (indexGraphName != null) {

            var indexSubject = indexGraphName.get(subject);
            if (indexSubject != null) {
                return indexSubject.get(property);
            }
        }

        return null;
    }

    public Map<String, JsonValue> get(String graphName, String subject) {

        var indexGraphName = index.get(graphName);

        if (indexGraphName != null) {
            return indexGraphName.get(subject);
        }

        return null;
    }

    public boolean contains(String graphName, String subject) {
        var indexGraphName = index.get(graphName);

        if (indexGraphName != null) {
            return indexGraphName.containsKey(subject);
        }

        return false;

    }

    public boolean contains(String graphName, String subject, String property) {
       var indexGraphName = index.get(graphName);

        if (indexGraphName != null) {
            var indexSubject = indexGraphName.get(subject);
            if (indexSubject != null) {
                return indexSubject.containsKey(property);
            }
        }
        return false;

    }

    public Optional<Map<String, Map<String, JsonValue>>> get(String graphName) {
        return Optional.ofNullable(index.get(graphName));
    }

    public String createIdentifier(String name) {
        return generator.createIdentifier(name);
    }

    public String createIdentifier() {
        return generator.createIdentifier();
    }

    public Collection<String> graphs() {
        return index.keySet();
    }

    public Collection<String> subjects(String graphName) {
        return index.getOrDefault(graphName, Collections.emptyMap()).keySet();
    }

    public Collection<String> properties(String graphName, String subject) {
        return index.getOrDefault(graphName, Collections.emptyMap()).getOrDefault(subject, Collections.emptyMap()).keySet();
    }

    /**
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#merge-node-maps">Merge Node Maps</a>
     */
    public void merge() {

        // 1.
        final NodeMap result = new NodeMap();

        // 2.
        for (final Map.Entry<String, Map<String, Map<String, JsonValue>>> graphEntry : index.entrySet()) {

            for (final Map.Entry<String, Map<String, JsonValue>> subject : graphEntry.getValue().entrySet()) {

                // 2.1.
                if (!result.contains(Keywords.MERGED, subject.getKey())) {
                    result.set(Keywords.MERGED, subject.getKey(), Keywords.ID, JsonProvider.instance().createValue(subject.getKey()));
                }

                // 2.2.
                for (final Map.Entry<String, JsonValue> property : subject.getValue().entrySet()) {

                    // 2.2.1.
                    if (!Keywords.TYPE.equals(property.getKey())
                            && Keywords.matchForm(property.getKey())
                    ) {

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), property.getValue());

                    } else {

                        final JsonArrayBuilder array;

                        if (result.contains(Keywords.MERGED, subject.getKey(), property.getKey())) {
                            array = JsonProvider.instance().createArrayBuilder(JsonUtils.toJsonArray(result.get(Keywords.MERGED, subject.getKey(), property.getKey())));

                        } else {
                            array = JsonProvider.instance().createArrayBuilder();
                        }

                        JsonUtils.toJsonArray(property.getValue()).forEach(array::add);

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), array.build());
                    }

                }

            }
        }

        if (result.index.get(Keywords.MERGED) != null) {
            index.put(Keywords.MERGED, result.index.get(Keywords.MERGED));
        }
    }

    public boolean contains(String id) {
        return index.containsKey(id);
    }

    @Override
    public String toString() {
        return Objects.toString(index);
    }
}
