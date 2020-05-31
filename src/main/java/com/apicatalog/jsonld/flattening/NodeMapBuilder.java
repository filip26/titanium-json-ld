package com.apicatalog.jsonld.flattening;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
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
        Map<String, JsonValue> graph = nodeMap.get(activeGraph).asJsonObject();
        
        Map<String, JsonValue> subjectNode = null;
        
        if (activeSubject != null) {
            subjectNode = graph.get(activeSubject).asJsonObject();
        }
        
        JsonObject elementObject = element.asJsonObject();
        
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
                    subjectNode.put(activeProperty, Json.createArrayBuilder().add(elementObject).build());
                }
                
                // 3.
                
                
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
        } else if (NodeObject.isNodeObject(elementObject)) {
            
        }
    }
    
}


