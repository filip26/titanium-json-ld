package com.apicatalog.jsonld.flattening;

import java.util.LinkedHashMap;
import java.util.Map;

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
    
    
    // optional
    private String activeGraph;
    private String activeSubject;
    private String activeProperty;
    private Map list;   //TODO
    
    
    private NodeMapBuilder(final JsonStructure element, final Map<String, JsonValue> nodeMap) {
        this.element = element;
        this.nodeMap = nodeMap;
        
        // default values
        this.activeGraph = Keywords.DEFAULT;
        this.activeSubject = null;
        this.activeProperty = null;
        this.list = null;
    }
    
    public static final NodeMapBuilder with(final JsonStructure element, final Map<String, JsonValue> nodeMap) {
        return new NodeMapBuilder(element, nodeMap);
    }
    
    public void build() throws JsonLdError {
        
        // 1.
        if (JsonUtils.isArray(element)) {
            // 1.1.
            //TODO
            return;
        }
        
        // 2.
        //TODO
        
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
                
                //TODO
                
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


