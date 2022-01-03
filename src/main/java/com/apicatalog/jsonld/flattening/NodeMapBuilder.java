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

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.Utils;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public final class NodeMapBuilder {

    // required
    private JsonStructure element;
    private final NodeMap nodeMap;

    // optional
    private String activeGraph;
    private String activeSubject;
    private String activeProperty;
    private Map<String, JsonValue> referencedNode;
    private Map<String, JsonValue> list;


    private NodeMapBuilder(final JsonStructure element, final NodeMap nodeMap) {
        this.element = element;
        this.nodeMap = nodeMap;

        // default values
        this.activeGraph = Keywords.DEFAULT;
        this.activeSubject = null;
        this.activeProperty = null;
        this.list = null;
        this.referencedNode = null;
    }

    public static final NodeMapBuilder with(final JsonStructure element, final NodeMap nodeMap) {
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

    public NodeMapBuilder list(Map<String, JsonValue> list) {
        this.list = list;
        return this;
    }

    public NodeMapBuilder referencedNode(Map<String, JsonValue> referencedNode) {
        this.referencedNode = referencedNode;
        return this;
    }

    public NodeMap build() throws JsonLdError {

        // 1.
        if (JsonUtils.isArray(element)) {

            // 1.1.
            for (JsonValue item : element.asJsonArray()) {

                JsonStructure itemValue;

                if (JsonUtils.isObject(item)) {
                    itemValue = item.asJsonObject();

                } else if (JsonUtils.isArray(item)) {
                    itemValue = item.asJsonArray();

                } else {
                    throw new IllegalStateException();
                }

                NodeMapBuilder
                    .with(itemValue, nodeMap)
                    .activeGraph(activeGraph)
                    .activeProperty(activeProperty)
                    .activeSubject(activeSubject)
                    .list(list)
                    .referencedNode(referencedNode)
                    .build();

            }
            return nodeMap;
        }

        // 2.
        final Map<String, JsonValue> elementObject = new LinkedHashMap<>(element.asJsonObject());

        // 3.
        if (elementObject.containsKey(Keywords.TYPE)) {

            final JsonArrayBuilder types = Json.createArrayBuilder();

            // 3.1.
            JsonUtils.toStream(elementObject.get(Keywords.TYPE))
                    .map(item -> JsonUtils.isString(item) && BlankNode.hasPrefix(((JsonString)item).getString())
                                    ? Json.createValue(nodeMap.createIdentifier(((JsonString)item).getString()))
                                    : item
                            )
                    .forEach(types::add);

            elementObject.put(Keywords.TYPE, types.build());
        }

        // 4.
        if (elementObject.containsKey(Keywords.VALUE)) {

            // 4.1.
            if (list == null) {

                // 4.1.1.
                if (nodeMap.contains(activeGraph, activeSubject, activeProperty)) {

                    final JsonArray activePropertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty).asJsonArray();

                    if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, element))) {
                        nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder(activePropertyValue).add(element).build());
                    }

                // 4.1.2.
                } else {
                    nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(elementObject)).build());
                }

            // 4.2.
            } else {
                list.put(Keywords.LIST, Json.createArrayBuilder(list.get(Keywords.LIST).asJsonArray()).add(element).build());
            }

        // 5.
        } else if (elementObject.containsKey(Keywords.LIST)) {

            // 5.1.
            Map<String, JsonValue> result = new LinkedHashMap<>();
            result.put(Keywords.LIST, JsonValue.EMPTY_JSON_ARRAY);

            // 5.2.
            NodeMapBuilder
                    .with((JsonStructure)elementObject.get(Keywords.LIST), nodeMap)
                    .activeGraph(activeGraph)
                    .activeSubject(activeSubject)
                    .activeProperty(activeProperty)
                    .referencedNode(referencedNode)
                    .list(result)
                    .build();


            // 5.3.
            if (list == null) {

                if (nodeMap.contains(activeGraph, activeSubject, activeProperty)) {

                    nodeMap.set(activeGraph, activeSubject, activeProperty,
                                Json.createArrayBuilder(
                                        nodeMap.get(activeGraph, activeSubject, activeProperty)
                                            .asJsonArray())
                                            .add(JsonUtils.toJsonObject(result))
                                            .build()
                                );

                } else {
                    nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(result)).build());
                }

            // 5.4.
            } else {
                list.put(Keywords.LIST, Json.createArrayBuilder(list.get(Keywords.LIST).asJsonArray()).add(JsonUtils.toJsonObject(result)).build());
            }

        // 6.
        } else if (NodeObject.isNodeObject(element)) {

            String id = null;

            // 6.1.
            if (elementObject.containsKey(Keywords.ID)) {

                final JsonValue idValue = elementObject.get(Keywords.ID);

                if (JsonUtils.isNull(idValue) || JsonUtils.isNotString(idValue)) {
                    return nodeMap;
                }

                id = ((JsonString)idValue).getString();

                if (BlankNode.hasPrefix(id)) {
                    id = nodeMap.createIdentifier(id);
                }
                elementObject.remove(Keywords.ID);

            // 6.2.
            } else {
                id = nodeMap.createIdentifier();
            }

            // 6.3.
            if (id != null && !nodeMap.contains(activeGraph, id)) {
                nodeMap.set(activeGraph, id, Keywords.ID, Json.createValue(id));
            }

            // 6.4.

            // 6.5.
            if (referencedNode != null) {

                // 6.5.1.
                if (nodeMap.contains(activeGraph, id, activeProperty)) {

                    final JsonArray activePropertyValue = nodeMap.get(activeGraph, id, activeProperty).asJsonArray();

                    if (activePropertyValue.stream().filter(JsonUtils::isObject).noneMatch(e -> Objects.equals(e.asJsonObject(), JsonUtils.toJsonObject(referencedNode)))) {
                        nodeMap.set(activeGraph, id, activeProperty, Json.createArrayBuilder(activePropertyValue).add(JsonUtils.toJsonObject(referencedNode)).build());
                    }

                // 6.5.2.
                } else {
                    nodeMap.set(activeGraph, id, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(referencedNode)).build());
                }

            // 6.6.
            } else if (activeProperty != null) {

                // 6.6.1.
                final JsonObject reference = Json.createObjectBuilder().add(Keywords.ID, id).build();

                // 6.6.2.
                if (list == null) {

                    // 6.6.2.2.
                    if (nodeMap.contains(activeGraph, activeSubject, activeProperty)) {

                        final JsonArray activePropertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty).asJsonArray();

                        if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, reference))) {
                            nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder(activePropertyValue).add(reference).build());
                        }

                    // 6.6.2.1.
                    } else {
                        nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(reference).build());
                    }

                // 6.6.3.
                } else {
                    list.put(Keywords.LIST, Json.createArrayBuilder(list.get(Keywords.LIST).asJsonArray()).add(reference).build());
                }
            }

            // 6.7.
            if (elementObject.containsKey(Keywords.TYPE)) {

                final Set<JsonValue> nodeType = new LinkedHashSet<>();

                final JsonValue nodeTypeValue = nodeMap.get(activeGraph, id, Keywords.TYPE);

                if (JsonUtils.isArray(nodeTypeValue)) {
                    nodeTypeValue.asJsonArray().stream().filter(JsonUtils::isNotNull).forEach(nodeType::add);

                } else if (JsonUtils.isNotNull(nodeTypeValue)) {
                    nodeType.add(nodeTypeValue);
                }

                final JsonValue typeValue = elementObject.get(Keywords.TYPE);

                if (JsonUtils.isArray(typeValue)) {
                    typeValue.asJsonArray().stream().filter(JsonUtils::isNotNull).forEach(nodeType::add);

                } else if (JsonUtils.isNotNull(typeValue)) {
                    nodeType.add(typeValue);
                }

                final JsonArrayBuilder nodeTypeBuilder = Json.createArrayBuilder();
                nodeType.forEach(nodeTypeBuilder::add);

                nodeMap.set(activeGraph, id, Keywords.TYPE, nodeTypeBuilder.build());

                elementObject.remove(Keywords.TYPE);
            }

            // 6.8.
            if (elementObject.containsKey(Keywords.INDEX)) {

                if (nodeMap.contains(activeGraph, id, Keywords.INDEX)) {
                    throw new JsonLdError(JsonLdErrorCode.CONFLICTING_INDEXES);
                }

                nodeMap.set(activeGraph, id, Keywords.INDEX, elementObject.get(Keywords.INDEX));
                elementObject.remove(Keywords.INDEX);
            }

            // 6.9.
            if (elementObject.containsKey(Keywords.REVERSE)) {

                // 6.9.1.
                Map<String, JsonValue> referenced = new LinkedHashMap<>();
                referenced.put(Keywords.ID, Json.createValue(id));

                // 6.9.2.
                JsonValue reverseMap = elementObject.get(Keywords.REVERSE);

                // 6.9.3.
                for (Entry<String, JsonValue> entry : reverseMap.asJsonObject().entrySet()) {

                    // 6.9.3.1.
                    for (JsonValue value : entry.getValue().asJsonArray()) {

                        // 6.9.3.1.1.
                        NodeMapBuilder
                            .with((JsonStructure)value, nodeMap)
                            .activeGraph(activeGraph)
                            .referencedNode(referenced)
                            .activeProperty(entry.getKey())
                            .build();
                    }
                }

                // 6.9.4.
                elementObject.remove(Keywords.REVERSE);
            }

            // 6.10.
            if (elementObject.containsKey(Keywords.GRAPH)) {

                NodeMapBuilder
                    .with((JsonStructure)elementObject.get(Keywords.GRAPH), nodeMap)
                    .activeGraph(id)
                    .build();

                elementObject.remove(Keywords.GRAPH);
            }

            // 6.11.
            if (elementObject.containsKey(Keywords.INCLUDED)) {

                NodeMapBuilder
                    .with((JsonStructure)elementObject.get(Keywords.INCLUDED), nodeMap)
                    .activeGraph(activeGraph)
                    .build();

                elementObject.remove(Keywords.INCLUDED);
            }

            // 6.12.
            for (String property : Utils.index(elementObject.keySet(), true)) {

                final JsonValue value = elementObject.get(property);

                // ignore invalid expanded values - see expansion test #122
                if (value == null || !ValueType.ARRAY.equals(value.getValueType()) && !ValueType.OBJECT.equals(value.getValueType())) {
                    continue;
                }

                // 6.12.1.
                if (BlankNode.hasPrefix(property)) {
                    property = nodeMap.createIdentifier(property);
                }

                // 6.12.2.
                if (!nodeMap.contains(activeGraph, id, property)) {
                    nodeMap.set(activeGraph, id, property, JsonValue.EMPTY_JSON_ARRAY);
                }

                // 6.12.3.
                NodeMapBuilder
                        .with((JsonStructure)value, nodeMap)
                        .activeGraph(activeGraph)
                        .activeSubject(id)
                        .activeProperty(property)
                        .build();
            }
        }
        return nodeMap;
    }
}
