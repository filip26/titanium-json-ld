package com.apicatalog.jsonld.flattening;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

public final class NodeMap {

    private final Map<String, Map<String, Map<String, JsonValue>>> nodeMap;
    
    public NodeMap() {
        this.nodeMap = new LinkedHashMap<>();
        this.nodeMap.put(Keywords.DEFAULT, new LinkedHashMap<>());
    }
    
    public boolean doesNotContain(String activeGraph, String activeSubject, String activeProperty) {
        return !nodeMap.containsKey(activeGraph) 
                    || !nodeMap.get(activeGraph).containsKey(activeSubject)
                    || !nodeMap.get(activeGraph).get(activeSubject).containsKey(activeProperty)
                    ;
    }

    public void set(String activeGraph, String activeSubject, String activeProperty, JsonValue value) {

        if (activeSubject == null) {
            return;
        }

        if (!nodeMap.containsKey(activeGraph)) {
            nodeMap.put(activeGraph, new LinkedHashMap<>());
        }
        
        if (!nodeMap.get(activeGraph).containsKey(activeSubject)) {
            nodeMap.get(activeGraph).put(activeSubject, new LinkedHashMap<>());
        }
        
        nodeMap.get(activeGraph).get(activeSubject).put(activeProperty, value);
    }

    public JsonValue get(String activeGraph, String activeSubject, String activeProperty) {
        
        if (nodeMap.containsKey(activeGraph) && nodeMap.get(activeGraph).containsKey(activeSubject)) {
            return nodeMap.get(activeGraph).get(activeSubject).get(activeProperty);
        }
        
        return null;
    }

    public boolean doesNotContain(String activeGraph, String activeSubject) {
        return !nodeMap.containsKey(activeGraph) 
                || !nodeMap.get(activeGraph).containsKey(activeSubject)
                ;
    }

    public Map<String, JsonObject> get(String activeGraph) {
        
        if (!nodeMap.containsKey(activeGraph)) {
            return null;
        }

        return nodeMap
                    .get(activeGraph)
                    .entrySet()
                    .stream()
                    .map(e -> new Object[] {e.getKey(), JsonUtils.toJsonObject(e.getValue())})
                    .collect(Collectors.toMap(e -> (String)e[0], e -> (JsonObject)e[1]))                    
                    ;
    }

    public Collection<String> keys(boolean ordered) {
        return ordered 
                    ? nodeMap.keySet().stream().sorted().collect(Collectors.toList())
                    : nodeMap.keySet()
                    ;
    }
    
}
