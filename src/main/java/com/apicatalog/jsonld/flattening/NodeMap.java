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
//        .put(property, cloneValue(value));
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

    public void merge1() {

        final NodeMap result = new NodeMap();

        for (final var graphEntry : index.entrySet()) {

            final String graphName = graphEntry.getKey();

            for (final var subjectEntry : graphEntry.getValue().entrySet()) {
                final String subjectId = subjectEntry.getKey();

                // Create merged entry if missing
                if (result.find(Keywords.MERGED, subjectId).isEmpty()) {
                    result.set(Keywords.MERGED, subjectId, Keywords.ID, subjectId);
                }

                // If this is a blank-node graph (e.g. _:b0), do NOT merge by subject ID
                // Use a scoped key to avoid merging distinct nodes from blank graphs
                final boolean isBlankGraph = graphName.startsWith("_:");

                final String targetSubjectKey = isBlankGraph
                        ? graphName + "::" + subjectId
                        : subjectId;

                if (result.find(Keywords.MERGED, targetSubjectKey).isEmpty()) {
                    result.set(Keywords.MERGED, targetSubjectKey, Keywords.ID, subjectId);
                }

                for (final var property : subjectEntry.getValue().entrySet()) {

                    final String prop = property.getKey();

                    // For blank graphs, isolate them in their scoped key
                    final String target = isBlankGraph ? targetSubjectKey : subjectId;

                    if (!Keywords.TYPE.equals(prop) && Keywords.matchForm(prop)) {
                        result.set(Keywords.MERGED, target, prop, property.getValue());
                        continue;
                    }

                    final List<Object> mergedValues;
                    var existing = result.get(Keywords.MERGED, target, prop);

                    if (existing instanceof Collection<?> col) {
                        mergedValues = (col instanceof ArrayList array)
                                ? array
                                : new ArrayList<>(col);
                    } else {
                        mergedValues = new ArrayList<>();
                        result.set(Keywords.MERGED, target, prop, mergedValues);
                    }

                    if (property.getValue() instanceof Collection<?> col) {
                        mergedValues.addAll(col);
                    } else {
                        mergedValues.add(property.getValue());
                    }
                }
            }
        }

        if (result.index.get(Keywords.MERGED) != null) {
            index.put(Keywords.MERGED, result.index.get(Keywords.MERGED));
        }
    }

    public void mergeX() {
        final NodeMap result = new NodeMap();

        for (final var graphEntry : index.entrySet()) {
            final String graphName = graphEntry.getKey();
            final var graph = graphEntry.getValue();

            for (final var subjectEntry : graph.entrySet()) {
                final String subject = subjectEntry.getKey();
                final Map<String, Object> properties = subjectEntry.getValue();

                // 🔸 If this is not the default graph, the node must be nested under that
                // graph.
                final String mergedGraphName = Keywords.DEFAULT.equals(graphName)
                        ? Keywords.MERGED
                        : graphName; // preserve graph boundary

                // 1. Ensure @id
                result.set(mergedGraphName, subject, Keywords.ID, subject);

                // 2. Copy properties
                for (final var propEntry : properties.entrySet()) {
                    final String prop = propEntry.getKey();
                    final Object value = propEntry.getValue();

                    if (!Keywords.TYPE.equals(prop) && Keywords.matchForm(prop)) {
                        result.set(mergedGraphName, subject, prop, deepCopy(value));
                        continue;
                    }

                    List<Object> mergedValues;
                    Object existing = result.get(mergedGraphName, subject, prop);
                    if (existing instanceof Collection<?> values) {
                        mergedValues = (values instanceof ArrayList)
                                ? (ArrayList<Object>) values
                                : new ArrayList<>(values);
                    } else {
                        mergedValues = new ArrayList<>();
                        result.set(mergedGraphName, subject, prop, mergedValues);
                    }

                    if (value instanceof Collection<?> values) {
                        for (Object v : values)
                            mergedValues.add(deepCopy(v));
                    } else {
                        mergedValues.add(deepCopy(value));
                    }
                }
            }
        }

        // Combine graphs into @merged, but preserve separation
        for (final var e : result.index.entrySet()) {
            if (Keywords.MERGED.equals(e.getKey()))
                continue;
            if (Keywords.DEFAULT.equals(e.getKey()))
                continue;
            result.index
                    .computeIfAbsent(Keywords.MERGED, x -> new LinkedHashMap<>())
                    .putAll(e.getValue());
        }

        index.put(Keywords.MERGED, result.index.get(Keywords.MERGED));
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

                    if (property.getKey().equals("ex:name")) {
                        System.out.println("1 >>> " + subject.getKey() + ", " + property + ", " + graphEntry.getKey() );
                        System.out.println("2 >>> " +result.get(Keywords.MERGED, subject.getKey(), property.getKey()));
                    }
                    
                    // 2.2.1.
                    if (!Keywords.TYPE.equals(property.getKey()) && Keywords.matchForm(property.getKey())) {

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), property.getValue());

                    } else {

                        final List<Object> mergedValues;

                        if (result.get(Keywords.MERGED, subject.getKey(), property.getKey()) instanceof Collection<?> values) {

                            if (values instanceof ArrayList list) {
                                mergedValues = list;

                            } else {
                                mergedValues = new ArrayList<Object>(values);
                                result.set(Keywords.MERGED, subject.getKey(), property.getKey(), mergedValues);
                            }

                        } else {
                            mergedValues = new ArrayList<>();
                            result.set(Keywords.MERGED, subject.getKey(), property.getKey(), mergedValues);
                        }

                        System.out.println("######     " + property.getValue());
                        
                        if (property.getValue() instanceof Collection<?> properties) {

                            properties.forEach(p -> mergedValues.add(deepCopy(p)));
//                            mergedValues.addAll(properties);

                        } else {
                            mergedValues.add(deepCopy(property.getValue()));
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

    @SuppressWarnings("unchecked")
    private static Object deepCopy(Object value) {
        if (value instanceof Map<?, ?> map) {
            Map<String, Object> clone = new LinkedHashMap<>();
            map.forEach((k, v) -> clone.put(String.valueOf(k), deepCopy(v)));
            return clone;
        } else if (value instanceof List<?> list) {
            List<Object> clone = new ArrayList<>(list.size());
            for (Object v : list) {
                clone.add(deepCopy(v));
            }
            return clone;
        }
        return value; // primitives, strings, etc.
    }

}