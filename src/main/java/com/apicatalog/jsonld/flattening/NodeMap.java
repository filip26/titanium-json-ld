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
    
    private final BlankNodeIdGenerator generator = new BlankNodeIdGenerator();
    
    public NodeMap() {
        this.index = new LinkedHashMap<>();
        this.index.put(Keywords.DEFAULT, new LinkedHashMap<>());
    }
    
    public boolean doesNotContain(String graphName, String subject, String property) {
        return !index.containsKey(graphName) 
                    || !index.get(graphName).containsKey(subject)
                    || !index.get(graphName).get(subject).containsKey(property)
                    ;
    }

    public void set(String graphName, String subject, String property, JsonValue value) {

        if (subject == null) {
            return;
        }

        if (!index.containsKey(graphName)) {
            index.put(graphName, new LinkedHashMap<>());
        }
        
        if (!index.get(graphName).containsKey(subject)) {
            index.get(graphName).put(subject, new LinkedHashMap<>());
        }
        
        index.get(graphName).get(subject).put(property, value);
    }

    public JsonValue get(String graphName, String subject, String property) {
        
        if (index.containsKey(graphName) && index.get(graphName).containsKey(subject)) {
            return index.get(graphName).get(subject).get(property);
        }
        
        return null;
    }

    public boolean doesNotContain(String graphName, String subject) {
        return !index.containsKey(graphName) 
                || !index.get(graphName).containsKey(subject)
                ;
    }

    public Map<String, JsonObject> get(String graphName) {
        
        if (!index.containsKey(graphName)) {
            return null;
        }

        return index
                    .get(graphName)
                    .entrySet()
                    .stream()
                    .map(e -> new Object[] {e.getKey(), JsonUtils.toJsonObject(e.getValue())})
                    .collect(Collectors.toMap(e -> (String)e[0], e -> (JsonObject)e[1]))                    
                    ;
    }

    public String createIdentifier(String name) {
        return generator.createIdentifier(name);
    }

    public String createIdentifier() {
        return generator.createIdentifier();
    }

    public Collection<String> graphs(boolean sorted) {
        return sorted 
                    ? index.keySet().stream().sorted().collect(Collectors.toList())
                    : index.keySet()
                    ;
    }

    public Collection<String> subjects(String graphName, boolean sorted) {
        return sorted 
                ? index.get(graphName).keySet().stream().sorted().collect(Collectors.toList())
                : index.get(graphName).keySet()
                ;
    }

    public Collection<String> properties(String graphName, String subject, boolean sorted) {
        return sorted 
                ? index.get(graphName).get(subject).keySet().stream().sorted().collect(Collectors.toList())
                : index.get(graphName).get(subject).keySet()
                ;
    }
    
}
