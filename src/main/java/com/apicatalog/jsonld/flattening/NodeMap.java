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
import java.util.Collections;
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

        if (subject == null) { // TODO ?!
            return;
        }

        index.computeIfAbsent(graphName, x -> new LinkedHashMap<>())
                .computeIfAbsent(subject, x -> new LinkedHashMap<>())
                .put(property, value);
    }

    public Optional<Map<String, Map<String, Object>>> get(String graphName) {
        return Optional.ofNullable(index.get(graphName));
    }

    // TODO Optional for all get or no? and if yes, then find or get?
    public Optional<Map<String, ?>> get(String graphName, String subject) {
        return Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject));
//                .orElse(null);
//        if (index.containsKey(graphName)) {
//            return index.get(graphName).get(subject);
//        }
//
//        return null;
    }

    public Object get(String graphName, String subject, String property) {
        return Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject))
                .map(s -> s.get(property))
                .orElse(null);
//        if (index.containsKey(graphName) && index.get(graphName).containsKey(subject)) {
//            return index.get(graphName).get(subject).get(property);
//        }
//
//        return null;
    }

//    public boolean contains(String graphName, String subject) {
//        return Optional.ofNullable(index.get(graphName))
//                    .map(g -> g.containsKey(subject))
//                    .orElse(false);
////        return index.containsKey(graphName) && index.get(graphName).containsKey(subject);
//    }
//
    public boolean contains(String graphName, String subject, String property) {
        return Optional.ofNullable(index.get(graphName))
                .map(g -> g.get(subject))
                .map(s -> s.containsKey(property))
                .orElse(false);
////        return index.containsKey(graphName)
////                && index.get(graphName).containsKey(subject)
////                && index.get(graphName).get(subject).containsKey(property);
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
        return index.getOrDefault(graphName, Collections.emptyMap())
                .getOrDefault(subject, Collections.emptyMap())
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
                if (result.get(Keywords.MERGED, subject.getKey()).isEmpty()) {
                    result.set(
                            Keywords.MERGED, subject.getKey(),
                            Keywords.ID, subject.getKey());
                }

                // 2.2.
                for (final var property : subject.getValue().entrySet()) {

                    // 2.2.1.
                    if (!Keywords.TYPE.equals(property.getKey())
                            && Keywords.matchForm(property.getKey())) {

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), property.getValue());

                    } else {

                        final List<Object> array;

                        if (result.get(Keywords.MERGED, subject.getKey(), property.getKey()) instanceof Collection<?> col) {
                            array = new ArrayList<Object>(col);

                        } else {
                            array = new ArrayList<>();
                        }

                        if (property.getValue() instanceof Collection<?> col) {
                            array.addAll(col);
                        } else {
                            array.add(property.getValue());
                        }

//                        JsonUtils.toJsonArray(property.getValue()).forEach(array::add);

                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), array);
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