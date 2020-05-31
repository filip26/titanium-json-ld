package com.apicatalog.jsonld.flattening;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.grammar.CompactUri;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.NodeObject;
import com.apicatalog.jsonld.json.JsonUtils;

public final class NodeMapBuilder {

    // required
    private JsonStructure element;
    private Map<String, JsonValue> nodeMap;
    private final BlankNodeIdGenerator idGenerator;
    
    
    // optional
    private String activeGraph;
    private String activeSubject;
    private String activeProperty;
    private Map list;   //TODO
    
    
    private NodeMapBuilder(final JsonStructure element, final Map<String, JsonValue> nodeMap, final BlankNodeIdGenerator idGenerator) {
        this.element = element;
        this.nodeMap = nodeMap;
        this.idGenerator = idGenerator;
        
        // default values
        this.activeGraph = Keywords.DEFAULT;
        this.activeSubject = null;
        this.activeProperty = null;
        this.list = null;
    }
    
    public static final NodeMapBuilder with(final JsonStructure element, final Map<String, JsonValue> nodeMap, final BlankNodeIdGenerator idGenerator) {
        return new NodeMapBuilder(element, nodeMap, idGenerator);
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
    
    public NodeMapBuilder list(Map list) {
        this.list = list;
        return this;
    }
    
    public void build() throws JsonLdError {
        
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
                    .with(itemValue, nodeMap, idGenerator)
                    .activeGraph(activeGraph)
                    .activeProperty(activeProperty)
                    .activeSubject(activeSubject)
                    .list(list)
                    .build();

            }
            return;
        }
        
        // 2.
        Map<String, JsonValue> graph = new LinkedHashMap<>(nodeMap.get(activeGraph).asJsonObject());
        
        Map<String, JsonValue> subjectNode = null;
        
        if (activeSubject != null) {
            subjectNode = new LinkedHashMap<>(graph.get(activeSubject).asJsonObject());
        }
        
        Map<String, JsonValue> elementObject = new LinkedHashMap<>(element.asJsonObject());
        
        // 3.
        if (elementObject.containsKey(Keywords.TYPE)) {
            
            // 3.1.
            for (JsonValue item : JsonUtils.toJsonArray(elementObject.get(Keywords.TYPE))) {
                //TODO
            }
            
        }
        
        // 4.
        if (elementObject.containsKey(Keywords.VALUE)) {

            // 4.1.
            if (list == null) {
                
                // 4.1.1.
                if (!subjectNode.containsKey(activeProperty)) {
                    subjectNode.put(activeProperty, Json.createArrayBuilder().add(JsonUtils.toJsonObject(elementObject)).build());
                    
                // 4.1.2.
                } else {
                    //TODO
                }

            // 4.2.
            } else {
                
                //TODO
                
            }
            
        // 5.
        } else if (elementObject.containsKey(Keywords.LIST)) {
        
            // 5.1.
            Map<String, JsonValue> result = new LinkedHashMap<>();
            result.put(Keywords.LIST, JsonValue.EMPTY_JSON_ARRAY);
            
            // 5.2.
            //TODO
            
            // 5.3.
            if (list == null) {
                //TODO
            // 5.4.
            } else {
                //TODO
            }
            
        // 6.
        } else if (NodeObject.isNodeObject(element)) {

            String id = null;
            
            // 6.1.
            if (elementObject.containsKey(Keywords.ID)) {
                
                id = ((JsonString)elementObject.get(Keywords.ID)).getString();
                
                if (CompactUri.isBlankNode(id)) {
                    id = idGenerator.createIdentifier(id);
                }
                
                //TODO
                elementObject.remove(Keywords.ID);

            // 6.2.
            } else {                
                id = idGenerator.createIdentifier();
            }
            
            // 6.3.
            if (!graph.containsKey(id)) {
                graph.put(id, Json.createObjectBuilder().add(Keywords.ID, id).build());
            }
            
            // 6.4.
            Map<String, JsonValue> node = new LinkedHashMap<>(graph.get(id).asJsonObject());
            
            // 6.5.
            //TODO
            
            // 6.6.
            //TODO
            
            // 6.7.
            if (elementObject.containsKey(Keywords.TYPE)) {
                //TODO
            }

            // 6.8.
            if (elementObject.containsKey(Keywords.INDEX)) {
                //TODO
            }

            // 6.9.
            if (elementObject.containsKey(Keywords.REVERSE)) {
                //TODO
            }

            // 6.10.
            if (elementObject.containsKey(Keywords.GRAPH)) {
                //TODO
            }

            // 6.11.
            if (elementObject.containsKey(Keywords.INCLUDED)) {
                //TODO
            }

            // 6.12.
            for (String property : elementObject.keySet().stream().sorted().collect(Collectors.toList())) {
                
                JsonValue value = elementObject.get(property);
                
                //TODO
            }
            
            graph.put(id, JsonUtils.toJsonObject(node));
        }
        
        if (subjectNode != null) {
            graph.put(activeSubject, JsonUtils.toJsonObject(subjectNode));
        }
        
        nodeMap.put(activeGraph, JsonUtils.toJsonObject(graph));
    }
    
}


