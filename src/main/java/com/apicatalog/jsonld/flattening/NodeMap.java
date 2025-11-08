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
package com.apicatalog.jsonld.flattening;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.apicatalog.jsonld.lang.Keywords;

public final class NodeMap {

    private final Map<String, Map<String, Map<String, Object>>> index;

    private final BlankNodeIdGenerator generator;

    public NodeMap() {
        this(new BlankNodeIdGenerator());
    }

    public NodeMap(BlankNodeIdGenerator generator) {
        this.index = new LinkedHashMap<>();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());
        this.generator = generator;
    }

    public void set(String graphName, String subject, String property, Object value) {
        index.computeIfAbsent(graphName, x -> new LinkedHashMap<>())
                .computeIfAbsent(subject, x -> new LinkedHashMap<>())
                .put(property, value);
    }

    public Optional<Map<String, Map<String, Object>>> find(String graphName) {
        return Optional.ofNullable(index.get(graphName));
    }

    public Optional<Map<String, ?>> find(String graphName, String subject) {
        return Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject));
    }

    public Object get(String graphName, String subject, String property) {

        var x = Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject))
                .map(s -> s.get(property))
                .orElse(null);

        return x;
    }

    public boolean contains(String graphName, String subject, String property) {
        return Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject))
                .map(s -> s.containsKey(property))
                .orElse(false);
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
        return index.getOrDefault(graphName, Map.of()).keySet();
    }

    public Collection<String> properties(String graphName, String subject) {
        return index.getOrDefault(graphName, Map.of())
                .getOrDefault(subject, Map.of())
                .keySet();
    }

    /**
     *
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#merge-node-maps">Merge
     *      Node Maps</a>
     */
    public void merge() {

        // 1.
        final NodeMap result = new NodeMap();

        // 2.
        for (final var graphEntry : index.entrySet()) {

            for (final var subject : graphEntry.getValue().entrySet()) {

                // 2.1.
                if (result.find(Keywords.MERGED, subject.getKey()).isEmpty()) {
                    result.set(
                            Keywords.MERGED, subject.getKey(),
                            Keywords.ID, subject.getKey());
                }

                // 2.2.
                for (final var property : subject.getValue().entrySet()) {

                    // 2.2.1.
                    if (!Keywords.TYPE.equals(property.getKey()) && Keywords.matchForm(property.getKey())) {

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), property.getValue());

                    } else {

                        final List<Object> mergedValues;

                        if (result.get(Keywords.MERGED, subject.getKey(), property.getKey()) instanceof Collection<?> values) {

                            if (values instanceof ArrayList list) {
                                
                                @SuppressWarnings("unchecked")
                                final var typedList = (List<Object>)list;
                                
                                mergedValues = typedList;

                            } else {
                                mergedValues = new ArrayList<Object>(values);
                                result.set(Keywords.MERGED, subject.getKey(), property.getKey(), mergedValues);
                            }

                        } else {
                            mergedValues = new ArrayList<>();
                            result.set(Keywords.MERGED, subject.getKey(), property.getKey(), mergedValues);
                        }

                        if (property.getValue() instanceof Collection<?> properties) {

                            mergedValues.addAll(properties);

                        } else {
                            mergedValues.add(property.getValue());
                        }
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