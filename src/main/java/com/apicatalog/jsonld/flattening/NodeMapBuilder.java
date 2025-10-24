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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.java.NativeAdapter;

public final class NodeMapBuilder {

    // required
    private final Object element; // collection or map
    private final NodeMap nodeMap;
    private final String activeGraph;

    // optional
    private String activeSubject;
    private String activeProperty;

    private Map<String, String> referencedNode;
    private Map<String, Collection<?>> list;

    public NodeMapBuilder(final Object element, final NodeMap nodeMap) {
        this(element, nodeMap, Keywords.DEFAULT);
    }

    public NodeMapBuilder(final Object element, final NodeMap nodeMap, final String activeGraph) {
        this.element = element;
        this.nodeMap = nodeMap;
        this.activeGraph = activeGraph;

        // default values
        this.activeSubject = null;
        this.activeProperty = null;
        this.list = null;
        this.referencedNode = null;
    }

    public NodeMapBuilder activeProperty(String activeProperty) {
        this.activeProperty = activeProperty;
        return this;
    }

    public NodeMapBuilder activeSubject(String activeSubject) {
        this.activeSubject = activeSubject;
        return this;
    }

    public NodeMapBuilder list(Map<String, Collection<?>> list) {
        this.list = list;
        return this;
    }

    public NodeMapBuilder referencedNode(Map<String, String> referencedNode) {
        this.referencedNode = referencedNode;
        return this;
    }

    public NodeMap build() throws JsonLdError {

        // 1.
        if (element instanceof Collection<?> elements) {

            // 1.1.
            for (var item : elements) {

                if (!(item instanceof Map) && !(item instanceof Collection)) {
                    throw new IllegalStateException();
                }

                new NodeMapBuilder(item, nodeMap, activeGraph)
                        .activeProperty(activeProperty)
                        .activeSubject(activeSubject)
                        .list(list)
                        .referencedNode(referencedNode)
                        .build();

            }
            return nodeMap;
        }

        if (!(element instanceof Map)) {
            throw new IllegalStateException();
        }

        // 2.
        @SuppressWarnings("unchecked")
        final var elementMap = new LinkedHashMap<>((Map<String, Object>) element);

        // 3.
        var elementTypeValue = elementMap.get(Keywords.TYPE);

        if (elementTypeValue != null) {

            final Collection<String> types;

            // 3.1.
            if (elementTypeValue instanceof Collection<?> values) {

                types = values.stream()
                        .map(String.class::cast)
                        .map(item -> BlankNode.hasPrefix(item)
                                ? nodeMap.createIdentifier(item)
                                : item)
                        .toList();

            } else if (elementTypeValue instanceof String value) {
                types = Set.of(BlankNode.hasPrefix(value) ? nodeMap.createIdentifier(value) : value);

            } else {
                throw new IllegalStateException();
            }

            elementMap.put(Keywords.TYPE, types);
        }

        // 4.
        if (elementMap.containsKey(Keywords.VALUE)) {

            // 4.1.
            if (list == null) {

                // 4.1.1.
                final var propertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty);

                if (propertyValue != null) {

                    if (propertyValue instanceof Collection<?> values) {

                        if (values.stream().noneMatch(element::equals)) {

                            if (propertyValue instanceof ArrayList array) {
                                @SuppressWarnings("unchecked")
                                var list = (List<Object>) array;
                                list.add(element);

                            } else {
                                var list = new ArrayList<Object>(values);
                                list.add(element);
                                nodeMap.set(
                                        activeGraph,
                                        activeSubject,
                                        activeProperty,
                                        list);
                            }
                        }

                    } else {
                        nodeMap.set(
                                activeGraph,
                                activeSubject,
                                activeProperty,
                                List.of(propertyValue, element));
                    }

                } else {
                    // 4.1.2.
                    nodeMap.set(
                            activeGraph,
                            activeSubject,
                            activeProperty,
                            Set.of(elementMap));
                }

            } else {
                // 4.2.
                list.put(Keywords.LIST, append(list.get(Keywords.LIST), element));
            }

            // 5.
        } else if (elementMap.containsKey(Keywords.LIST)) {

            // 5.1.
            final var result = new LinkedHashMap<String, Collection<?>>(Map.of(Keywords.LIST, List.of()));

            // 5.2.
            new NodeMapBuilder(elementMap.get(Keywords.LIST), nodeMap, activeGraph)
                    .activeSubject(activeSubject)
                    .activeProperty(activeProperty)
                    .referencedNode(referencedNode)
                    .list(result)
                    .build();

            // 5.3.
            if (list == null) {

                final var propertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty);

                if (propertyValue != null) {
                    nodeMap.set(
                            activeGraph,
                            activeSubject,
                            activeProperty,
                            append(propertyValue, result));

                } else {
                    nodeMap.set(
                            activeGraph,
                            activeSubject,
                            activeProperty,
                            Set.of(result));
                }

            } else {
                // 5.4.
                list.put(Keywords.LIST, append(list.get(Keywords.LIST), result));
            }

            // 6.
        } else if (JsonLdAdapter.isNode(element, NativeAdapter.instance())) {

            var id = (String) elementMap.get(Keywords.ID);

            // 6.1.
            if (id != null) {

                if (id instanceof String) {
                    if (BlankNode.hasPrefix(id)) {
                        id = nodeMap.createIdentifier(id);
                    }
                    elementMap.remove(Keywords.ID);

                } else {
                    return nodeMap;
                }

            } else {
                // 6.2.
                id = nodeMap.createIdentifier();
            }

            // 6.3.
            if (id != null && nodeMap.find(activeGraph, id).isEmpty()) {
                nodeMap.set(activeGraph, id, Keywords.ID, id);
            }

            // 6.4.

            // 6.5.
            if (referencedNode != null) {

                final var activePropertyValue = nodeMap.get(activeGraph, id, activeProperty);

                // 6.5.1.
                if (activePropertyValue != null) {

                    if (((Collection<?>) activePropertyValue).stream()
                            .filter(Map.class::isInstance)
                            .noneMatch(referencedNode::equals)) {

                        nodeMap.set(
                                activeGraph,
                                id,
                                activeProperty,
                                append(activePropertyValue, referencedNode));
                    }

                } else {
                    // 6.5.2.
                    nodeMap.set(activeGraph, id, activeProperty, Set.of(referencedNode));
                }

                // 6.6.
            } else if (activeProperty != null) {

                // 6.6.1.
                final var reference = Map.of(Keywords.ID, id);

                // 6.6.2.
                if (list == null) {

                    // 6.6.2.2.
                    final var activePropertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty);

                    if (activePropertyValue != null) {

                        if (((Collection<?>) activePropertyValue).stream().noneMatch(reference::equals)) {
                            nodeMap.set(
                                    activeGraph,
                                    activeSubject,
                                    activeProperty,
                                    append(activePropertyValue, reference));
                        }

                    } else {
                        // 6.6.2.1.
                        nodeMap.set(activeGraph, activeSubject, activeProperty, Set.of(reference));
                    }

                    // 6.6.3.
                } else {
                    list.put(Keywords.LIST, append(list.get(Keywords.LIST), reference));
                }
            }

            // 6.7.
            if (elementMap.containsKey(Keywords.TYPE)) {

                final var nodeType = new LinkedHashSet<>();

                final var nodeTypeValue = nodeMap.get(activeGraph, id, Keywords.TYPE);

                if (nodeTypeValue instanceof Collection<?> col) {
                    col.stream().filter(Predicate.not(Objects::isNull)).forEach(nodeType::add);

                } else if (nodeTypeValue != null) {
                    nodeType.add(nodeTypeValue);
                }

                final var typeValue = elementMap.get(Keywords.TYPE);

                if (typeValue instanceof Collection<?> values) {
                    values.stream()
                            .filter(Predicate.not(Objects::isNull))
                            .map(String.class::cast)
                            .forEach(nodeType::add);

                } else if (typeValue instanceof String value) {
                    nodeType.add(value);

                } else {
                    throw new IllegalStateException();
                }

                nodeMap.set(activeGraph, id, Keywords.TYPE, nodeType);

                elementMap.remove(Keywords.TYPE);
            }

            // 6.8.
            if (elementMap.containsKey(Keywords.INDEX)) {

                if (nodeMap.contains(activeGraph, id, Keywords.INDEX)) {
                    throw new JsonLdError(JsonLdErrorCode.CONFLICTING_INDEXES);
                }

                nodeMap.set(activeGraph, id, Keywords.INDEX, elementMap.get(Keywords.INDEX));
                elementMap.remove(Keywords.INDEX);
            }

            // 6.9.
            final var reverseMap = elementMap.get(Keywords.REVERSE);

            if (reverseMap != null) {

                // 6.9.1.
                final var referenced = Map.of(Keywords.ID, id);

                // 6.9.3.
                for (final var entry : ((Map<?, ?>) reverseMap).entrySet()) {

                    // 6.9.3.1.
                    for (final var value : (Collection<?>) entry.getValue()) {

                        // 6.9.3.1.1.
                        new NodeMapBuilder(value, nodeMap, activeGraph)
                                .referencedNode(referenced)
                                .activeProperty(entry.getKey().toString())
                                .build();
                    }
                }

                // 6.9.4.
                elementMap.remove(Keywords.REVERSE);
            }

            // 6.10.
            final var graphMap = elementMap.get(Keywords.GRAPH);

            if (graphMap != null) {

                new NodeMapBuilder(graphMap, nodeMap, id).build();

                elementMap.remove(Keywords.GRAPH);
            }

            // 6.11.

            final var includedMap = elementMap.get(Keywords.INCLUDED);

            if (includedMap != null) {

                new NodeMapBuilder(includedMap, nodeMap, activeGraph).build();

                elementMap.remove(Keywords.INCLUDED);
            }

            // 6.12.
            final var properties = elementMap.keySet().stream().sorted().iterator();

            while (properties.hasNext()) {

                var property = properties.next();

                final var value = elementMap.get(property);

                // ignore invalid expanded values - see expansion test #122
                if (value == null || !(value instanceof Map) && !(value instanceof Collection)) {
                    continue;
                }

                // 6.12.1.
                if (BlankNode.hasPrefix(property)) {
                    property = nodeMap.createIdentifier(property);
                }

                // 6.12.2.
                if (!nodeMap.contains(activeGraph, id, property)) {
                    nodeMap.set(activeGraph, id, property, List.of());
                }

                // 6.12.3.
                new NodeMapBuilder(value, nodeMap, activeGraph)
                        .activeSubject(id)
                        .activeProperty(property)
                        .build();
            }
        }
        return nodeMap;
    }

    @SuppressWarnings("unchecked")
    private static Collection<?> append(Object container, Object value) {

        if (container instanceof Collection<?> col) {

            if (col.isEmpty()) {
                return Set.of(value);
            }

            if (col instanceof ArrayList array) {
                array.add(value);
                return array;
            }

            final var array = new ArrayList<Object>(col);
            array.add(value);
            return array;
        }
        throw new IllegalStateException();
    }
}
