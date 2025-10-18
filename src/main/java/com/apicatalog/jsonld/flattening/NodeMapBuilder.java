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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.node.BlankNode;
import com.apicatalog.jsonld.node.NodeObject;
import com.apicatalog.tree.io.java.NativeAdapter;

public final class NodeMapBuilder {

    // required
    private final Object element; // collection or map
    private final NodeMap nodeMap;

    // optional
    private String activeGraph;
    private String activeSubject;
    private String activeProperty;
    private Map<String, String> referencedNode;
    private Map<String, Object> list;

    private NodeMapBuilder(final Object element, final NodeMap nodeMap) {
        this.element = element;
        this.nodeMap = nodeMap;

        // default values
        this.activeGraph = Keywords.DEFAULT;
        this.activeSubject = null;
        this.activeProperty = null;
        this.list = null;
        this.referencedNode = null;
    }

    public static final NodeMapBuilder with(final Object element, final NodeMap nodeMap) {
        return new NodeMapBuilder(element, nodeMap);
    }

    public NodeMapBuilder activeGraph(String activeGraph) {
        this.activeGraph = activeGraph;
        return this;
    }

    public NodeMapBuilder activeProperty(String activeProperty) {
        this.activeProperty = activeProperty;
        return this;
    }

    public NodeMapBuilder activeSubject(String activeSubject) {
        this.activeSubject = activeSubject;
        return this;
    }

    public NodeMapBuilder list(Map<String, Object> list) {
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

                NodeMapBuilder
                        .with(item, nodeMap)
                        .activeGraph(activeGraph)
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

                types = new ArrayList<String>(values.size());

                ((Collection<?>) elementTypeValue).stream()
                        .map(String.class::cast)
                        .map(item -> BlankNode.hasPrefix(item)
                                ? nodeMap.createIdentifier(item)
                                : item)
                        .forEach(types::add);

            } else if (elementTypeValue instanceof String value) {
                types = Set.of(BlankNode.hasPrefix(value) ? nodeMap.createIdentifier(value) : value);

            } else {
                throw new IllegalStateException();
            }

            elementMap.put(Keywords.TYPE, types);
        }

        // 4.
        if (elementMap.containsKey(Keywords.VALUE)) {
            System.out.println("value " + activeProperty + ", " + list + ", " + elementMap);
            System.out.println("      " + nodeMap.get(activeGraph, activeSubject, activeProperty));
            // 4.1.
            if (list == null) {

                // 4.1.1.
//                if (nodeMap.contains(activeGraph, activeSubject, activeProperty)) {

                if (nodeMap.get(activeGraph, activeSubject, activeProperty) instanceof Collection activePropertyValue) {
                    if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, element))) {
                        final var array = new ArrayList<Object>(activePropertyValue);
                        array.add(element);
                        nodeMap.set(activeGraph, activeSubject, activeProperty, array);
                    }
//                    }

//                    if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, element))) {
//                        nodeMap.set(activeGraph, activeSubject, activeProperty,
//                                JsonProvider.instance()
//                                        .createArrayBuilder(activePropertyValue).add(element).build());
//                    }

                    // 4.1.2.
                } else {
                    nodeMap.set(activeGraph, activeSubject, activeProperty, Set.of(elementMap));
//                            JsonProvider.instance()
//                                    .createArrayBuilder()
//                                    .add(JsonUtils.toJsonObject(elementObject))
//                                    .build());
                }

                // 4.2.
            } else {
                System.out.println("list " + element + ", " + append(list.get(Keywords.LIST), element));
                list.put(Keywords.LIST, append(list.get(Keywords.LIST), element));
            }

            // 5.
        } else if (elementMap.containsKey(Keywords.LIST)) {

            // 5.1.
            final var result = new LinkedHashMap<String, Object>();
            result.put(Keywords.LIST, Collections.emptyList());

            // 5.2.
            NodeMapBuilder
                    .with(elementMap.get(Keywords.LIST), nodeMap)
                    .activeGraph(activeGraph)
                    .activeSubject(activeSubject)
                    .activeProperty(activeProperty)
                    .referencedNode(referencedNode)
                    .list(result)
                    .build();

            // 5.3.
            if (list == null) {

                final var value = nodeMap.get(activeGraph, activeSubject, activeProperty);

                nodeMap.set(activeGraph, activeSubject, activeProperty, append(value, result));

//                if (value instanceof ArrayList array) {
//                    array.add(result);
//                    nodeMap.set(activeGraph, activeSubject, activeProperty, array);
////                    nodeMap.set(activeGraph, activeSubject, activeProperty,
////                            
////                            JsonProvider.instance().createArrayBuilder(
////                                    nodeMap.get(activeGraph, activeSubject, activeProperty)
////                                            .asJsonArray())
////                                    .add(JsonUtils.toJsonObject(result))
////                                    .build());
//
//                } else if (value instanceof Collection<?> col) {
//                    final var array = new ArrayList<Object>(col);
//                    array.add(result);
//                    nodeMap.set(activeGraph, activeSubject, activeProperty, array);
//
//                } else {
//                    nodeMap.set(activeGraph, activeSubject, activeProperty, Set.of(result));
//                }
//
            } else {
                // 5.4.
                System.out.println("2: " + list.get(Keywords.LIST) + ", " + result);
                list.put(Keywords.LIST, append(list.get(Keywords.LIST), result));
            }

            // 6.
        } else if (NodeObject.isNodeObject(element, NativeAdapter.instance())) {

            String id = (String) elementMap.get(Keywords.ID);

            // 6.1.
            if (id != null) {

//                if (JsonUtils.isNull(idValue) || JsonUtils.isNotString(idValue)) {
//                    return nodeMap;
//                }

//                id = ((JsonString) idValue).getString();

                if (BlankNode.hasPrefix(id)) {
                    id = nodeMap.createIdentifier(id);
                }
                elementMap.remove(Keywords.ID);

                // 6.2.
            } else {
                id = nodeMap.createIdentifier();
            }

            // 6.3.
            if (id != null && !nodeMap.contains(activeGraph, id)) {
                nodeMap.set(activeGraph, id, Keywords.ID, id);
            }

            // 6.4.

            // 6.5.
            if (referencedNode != null) {

                // 6.5.1.
                if (nodeMap.contains(activeGraph, id, activeProperty)) {

                    final Collection activePropertyValue = (Collection) nodeMap.get(activeGraph, id, activeProperty);

                    if (activePropertyValue.stream()
                            .filter(Map.class::isInstance)
                            .noneMatch(referencedNode::equals)) {
                        System.out.println("3 " + referencedNode + ", " + activePropertyValue + ", " + activeProperty);
                        nodeMap.set(
                                activeGraph,
                                id,
                                activeProperty,
                                append(activePropertyValue, referencedNode));
                    }

//                    if (activePropertyValue.stream().filter(JsonUtils::isObject).noneMatch(e -> Objects.equals(e.asJsonObject(),
//                            JsonUtils.toJsonObject(referencedNode)))) {
//                        nodeMap.set(activeGraph, id, activeProperty, JsonProvider.instance().createArrayBuilder(activePropertyValue)
//                                .add(JsonUtils.toJsonObject(referencedNode)).build());
//                    }

                    // 6.5.2.
                } else {
                    nodeMap.set(activeGraph, id, activeProperty, Set.of(referencedNode));
                }

                // 6.6.
            } else if (activeProperty != null) {

                // 6.6.1.
                final var reference = Map.of(Keywords.ID, id);
                System.out.println("active " + activeProperty + ", " + reference + ", " + list);
                // 6.6.2.
                if (list == null) {

                    // 6.6.2.2.
                    if (nodeMap.contains(activeGraph, activeSubject, activeProperty)) {

                        final Collection activePropertyValue = (Collection) nodeMap.get(activeGraph, activeSubject, activeProperty);

                        if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, reference))) {
                            nodeMap.set(activeGraph, activeSubject, activeProperty, append(activePropertyValue, reference));
                        }

                        // 6.6.2.1.
                    } else {
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

                if (nodeTypeValue instanceof Collection col) {
                    col.stream().filter(Predicate.not(Objects::isNull)).forEach(nodeType::add);

                } else if (nodeTypeValue != null) {
                    nodeType.add(nodeTypeValue);
                }

                final var typeValue = elementMap.get(Keywords.TYPE);
                System.out.println("type " + typeValue + ", " + nodeTypeValue);
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
                        NodeMapBuilder
                                .with(value, nodeMap)
                                .activeGraph(activeGraph)
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

                NodeMapBuilder
                        .with(graphMap, nodeMap)
                        .activeGraph(id)
                        .build();

                elementMap.remove(Keywords.GRAPH);
            }

            // 6.11.
            final var includedMap = elementMap.get(Keywords.INCLUDED);
            
            if (includedMap != null) {

                NodeMapBuilder
                        .with(includedMap, nodeMap)
                        .activeGraph(activeGraph)
                        .build();

                elementMap.remove(Keywords.INCLUDED);
            }

            // 6.12.
            final var properties = elementMap.keySet().stream().sorted().iterator();
            
            while (properties.hasNext()) {
                
                var property = properties.next();

                final var value = elementMap.get(property);
                
                System.out.println("prop " + property + " -> " + value);
                // ignore invalid expanded values - see expansion test #122
                if (value == null || !(value instanceof Map) && !(value instanceof Collection)) {
                    continue;
                }
//                if (value == null || 
//                        !ValueType.ARRAY.equals(value.getValueType()) 
//                        && !ValueType.OBJECT.equals(value.getValueType())) {
//                    continue;
//                }

                // 6.12.1.
                if (BlankNode.hasPrefix(property)) {
                    property = nodeMap.createIdentifier(property);
                }

                // 6.12.2.
                if (!nodeMap.contains(activeGraph, id, property)) {
//                    nodeMap.set(activeGraph, id, property, Collections.emptySet());
                }

                // 6.12.3.
                NodeMapBuilder
                        .with(value, nodeMap)
                        .activeGraph(activeGraph)
                        .activeSubject(id)
                        .activeProperty(property)
                        .build();
            }
        }
        return nodeMap;
    }

    static final Object append(Object container, Object value) {
         
        if (container instanceof ArrayList array) {
            array.add(value);
            return array;
//            nodeMap.set(activeGraph, activeSubject, activeProperty,
//                    
//                    JsonProvider.instance().createArrayBuilder(
//                            nodeMap.get(activeGraph, activeSubject, activeProperty)
//                                    .asJsonArray())
//                            .add(JsonUtils.toJsonObject(result))
//                            .build());
//        } lese
//            
        } else if (container == null) {
            if (value instanceof Collection) {
                return value;
            }
            return Set.of(value);
            
        } else if (value instanceof Collection<?> col) {
            final var array = new ArrayList<Object>(col);
            array.add(value);
            return array;

        } else if (value != null) {
            return List.of(container, value);

        } else {
            return Set.of(value);
        }
    }
}
/*
         if (container instanceof ArrayList array) {

            if (value instanceof Collection<?> values) {
                array.addAll(values);
                return array;

            } else if (value != null) {
                array.add(value);
                return array;

            }
            return container;

        } else if (container instanceof Collection<?> collection) {

            if (value == null) {
                return container;
            }

            final var array = new ArrayList<Object>(collection);

            if (value instanceof Collection<?> values) {
                array.addAll(values);
                return array;
            }

            array.add(value);
            return array;

        } else if (container != null) {

            if (value instanceof Collection<?> values) {
                var array = new ArrayList<Object>(1 + values.size());
                array.add(container);
                array.addAll(values);
                return array;
            }

            if (value != null) {
                return List.of(container, value);
            }

            return Set.of(container);
            
        }

//        if (value instanceof Collection) {
            return value;
//        }
//        return Set.of(value);

 */
 
