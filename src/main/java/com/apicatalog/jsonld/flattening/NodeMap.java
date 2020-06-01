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

    private final Map<String, Map<String, Map<String, JsonValue>>> index;
    
    public NodeMap() {
        this.index = new LinkedHashMap<>();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());
    }
    
    public boolean doesNotContain(String activeGraph, String activeSubject, String activeProperty) {
        return !index.containsKey(activeGraph) 
                    || !index.get(activeGraph).containsKey(activeSubject)
                    || !index.get(activeGraph).get(activeSubject).containsKey(activeProperty)
                    ;
    }

    public void set(String activeGraph, String activeSubject, String activeProperty, JsonValue value) {

        if (activeSubject == null) {
            return;
        }

        if (!index.containsKey(activeGraph)) {
            index.put(activeGraph, new LinkedHashMap<>());
        }
        
        if (!index.get(activeGraph).containsKey(activeSubject)) {
            index.get(activeGraph).put(activeSubject, new LinkedHashMap<>());
        }
        
        index.get(activeGraph).get(activeSubject).put(activeProperty, value);
    }

    public JsonValue get(String activeGraph, String activeSubject, String activeProperty) {
        
        if (index.containsKey(activeGraph) && index.get(activeGraph).containsKey(activeSubject)) {
            return index.get(activeGraph).get(activeSubject).get(activeProperty);
        }
        
        return null;
    }

    public boolean doesNotContain(String activeGraph, String activeSubject) {
        return !index.containsKey(activeGraph) 
                || !index.get(activeGraph).containsKey(activeSubject)
                ;
    }

    public Map<String, JsonObject> get(String activeGraph) {
        
        if (!index.containsKey(activeGraph)) {
            return null;
        }

        return index
                    .get(activeGraph)
                    .entrySet()
                    .stream()
                    .map(e -> new Object[] {e.getKey(), JsonUtils.toJsonObject(e.getValue())})
                    .collect(Collectors.toMap(e -> (String)e[0], e -> (JsonObject)e[1]))                    
                    ;
    }

    public Collection<String> keys(boolean ordered) {
        return ordered 
                    ? index.keySet().stream().sorted().collect(Collectors.toList())
                    : index.keySet()
                    ;
    }
    
}
