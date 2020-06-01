package com.apicatalog.jsonld.flattening;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.CompactUri;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.NodeObject;

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
        Map<String, JsonValue> elementObject = new LinkedHashMap<>(element.asJsonObject());
        
        // 3.
        if (elementObject.containsKey(Keywords.TYPE)) {
            
            JsonArrayBuilder types = Json.createArrayBuilder();
 
            // 3.1.
            for (JsonValue item : JsonUtils.toJsonArray(elementObject.get(Keywords.TYPE))) {
                
                if (JsonUtils.isString(item) && CompactUri.isBlankNode(((JsonString)item).getString())) {
                    types.add(Json.createValue(nodeMap.createIdentifier(((JsonString)item).getString())));
                    
                } else {
                    types.add(item);
                }
            }
            
            elementObject.put(Keywords.TYPE, types.build());
        }
        
        // 4.
        if (elementObject.containsKey(Keywords.VALUE)) {

            // 4.1.
            if (list == null) {
                
                // 4.1.1.
                if (nodeMap.doesNotContain(activeGraph, activeSubject, activeProperty)) {
                    nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(elementObject)).build());
                    
                // 4.1.2.
                } else {

                    JsonArray activePropertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty).asJsonArray();
                    
                    if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, element))) {
                        nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder(activePropertyValue).add(element).build());
                    }                    
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
                
                if (nodeMap.doesNotContain(activeGraph, activeSubject, activeProperty)) {
                    nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(result)).build());
                    
                } else {
                    
                    nodeMap.set(activeGraph, activeSubject, activeProperty,
                                Json.createArrayBuilder(
                                        nodeMap.get(activeGraph, activeSubject, activeProperty)
                                            .asJsonArray())
                                            .add(JsonUtils.toJsonObject(result))
                                            .build()
                                );
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
                
                id = ((JsonString)elementObject.get(Keywords.ID)).getString();
                
                if (CompactUri.isBlankNode(id)) {
                    id = nodeMap.createIdentifier(id);
                }
                
                elementObject.remove(Keywords.ID);

            // 6.2.
            } else {                
                id = nodeMap.createIdentifier();
            }
            
            // 6.3.
            if (nodeMap.doesNotContain(activeGraph, id)) {
                nodeMap.set(activeGraph, id, Keywords.ID, Json.createValue(id));
            }

            // 6.4.
            
            // 6.5.
            if (referencedNode != null) {
                
                // 6.5.1.
                if (nodeMap.doesNotContain(activeGraph, id, activeProperty)) {
                    
                    nodeMap.set(activeGraph, id, activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(referencedNode)).build());
                    
                // 6.5.2.
                } else {
                    JsonArray activePropertyValue = nodeMap.get(activeGraph, id, activeProperty).asJsonArray();

                    if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e,  JsonUtils.toJsonObject(referencedNode)))) {
                        nodeMap.set(activeGraph, id, activeProperty, Json.createArrayBuilder(activePropertyValue).add(JsonUtils.toJsonObject(referencedNode)).build());
                    }                        
                }
                
            // 6.6.
            } else if (activeProperty != null) {
                
                // 6.6.1.
                JsonObject reference = Json.createObjectBuilder().add(Keywords.ID, Json.createValue(id)).build();
                
                // 6.6.2.
                if (list == null) {

                    // 6.6.2.1.
                    if (nodeMap.doesNotContain(activeGraph, activeSubject, activeProperty)) {
                        nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder().add(reference).build());
                        
                    // 6.6.2.2.                        
                    } else {
                        JsonArray activePropertyValue = nodeMap.get(activeGraph, activeSubject, activeProperty).asJsonArray();

                        if (activePropertyValue.stream().noneMatch(e -> Objects.equals(e, reference))) {
                            nodeMap.set(activeGraph, activeSubject, activeProperty, Json.createArrayBuilder(activePropertyValue).add(reference).build());
                        }                        
                    }
                    
                // 6.6.3.                    
                } else {                    
                    list.put(Keywords.LIST, Json.createArrayBuilder(list.get(Keywords.LIST).asJsonArray()).add(reference).build());
                }
            }
            
            // 6.7.
            if (elementObject.containsKey(Keywords.TYPE)) {

                Set<JsonValue> nodeType = new LinkedHashSet<>();
                
                JsonValue nodeTypeValue = nodeMap.get(activeGraph, id, Keywords.TYPE);
                
                if (JsonUtils.isArray(nodeTypeValue)) {
                    
                    nodeTypeValue.asJsonArray().forEach(nodeType::add);

                } else if (JsonUtils.isNotNull(nodeTypeValue)) {
                    
                    nodeType.add(nodeTypeValue);    
                }
                
                JsonUtils.toJsonArray(elementObject.get(Keywords.TYPE)).forEach(nodeType::add);
                
                nodeMap.set(activeGraph, id, Keywords.TYPE, JsonUtils.toJsonArray(nodeType));
                
                elementObject.remove(Keywords.TYPE);
            }

            // 6.8.
            if (elementObject.containsKey(Keywords.INDEX)) {
                
                if (nodeMap.doesNotContain(activeGraph, id, Keywords.INDEX)) {
                    nodeMap.set(activeGraph, id, Keywords.INDEX, elementObject.get(Keywords.INDEX));

                    elementObject.remove(Keywords.INDEX);
                    
                } else {
                    throw new JsonLdError(JsonLdErrorCode.CONFLICTING_INDEXES);
                }
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
            for (String property : elementObject.keySet().stream().sorted().collect(Collectors.toList())) {
     
                JsonStructure value = (JsonStructure)elementObject.get(property);
           
                // 6.12.1.
                if (CompactUri.isBlankNode(property)) {
                    property = nodeMap.createIdentifier(property);
                }
                
                // 6.12.2.
                if (nodeMap.doesNotContain(activeGraph, id, property)) {
                    nodeMap.set(activeGraph, id, property, JsonValue.EMPTY_JSON_ARRAY);
                }

                // 6.12.3.
                NodeMapBuilder
                        .with(value, nodeMap)
                        .activeGraph(activeGraph)
                        .activeSubject(id)
                        .activeProperty(property)
                        .referencedNode(referencedNode)
                        .build();
            }
        }
        return nodeMap;
    }   
}
