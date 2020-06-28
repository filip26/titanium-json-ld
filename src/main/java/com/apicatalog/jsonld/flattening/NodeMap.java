package com.apicatalog.jsonld.flattening;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
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
        
        index
            .computeIfAbsent(graphName, x -> new LinkedHashMap<>())
            .computeIfAbsent(subject, x -> new LinkedHashMap<>())
            .put(property, value);
    }

    public JsonValue get(String graphName, String subject, String property) {
        
        if (index.containsKey(graphName) && index.get(graphName).containsKey(subject)) {
            return index.get(graphName).get(subject).get(property);
        }
        
        return null;
    }
    
    public Map<String, JsonValue> get(String graphName, String subject) {
        
        if (index.containsKey(graphName)) {
            return index.get(graphName).get(subject);
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

    public Collection<String> subjects(String graphName) {
        return subjects(graphName, false);
    }
    
    public Collection<String> subjects(String graphName, boolean sorted) {
        return sorted 
                ? index.getOrDefault(graphName, Collections.emptyMap()).keySet().stream().sorted().collect(Collectors.toList())
                : index.getOrDefault(graphName, Collections.emptyMap()).keySet()
                ;
    }

    public Collection<String> properties(String graphName, String subject, boolean sorted) {
        return sorted 
                ? index.getOrDefault(graphName, Collections.emptyMap()).getOrDefault(subject, Collections.emptyMap()).keySet().stream().sorted().collect(Collectors.toList())
                : index.getOrDefault(graphName, Collections.emptyMap()).getOrDefault(subject, Collections.emptyMap()).keySet()
                ;
    }
    
    /**
     * 
     * @see <a href="https://www.w3.org/TR/json-ld11-api/#merge-node-maps">Merge Node Maps</a>
     */
    public void merge() {
        
        // 1.
        final NodeMap result = new NodeMap();
 
        // 2.
        for (final Map.Entry<String, Map<String, Map<String, JsonValue>>> graphEntry : index.entrySet()) {
            
            for (final Map.Entry<String, Map<String, JsonValue>> subject : graphEntry.getValue().entrySet()) {
             
                // 2.1.
                if (result.doesNotContain(Keywords.MERGED, subject.getKey())) {
                    result.set(Keywords.MERGED, subject.getKey(), Keywords.ID, Json.createValue(subject.getKey()));
                }
                                
                // 2.2.
                for (final Map.Entry<String, JsonValue> property : subject.getValue().entrySet()) {
                
                    // 2.2.1.
                    if (!Keywords.TYPE.equals(property.getKey())
                            && Keywords.matchForm(property.getKey()) 
                            ) {
                        
                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), property.getValue());
                        
                    } else {
                        
                        final JsonArrayBuilder array;
                        
                        if (result.doesNotContain(Keywords.MERGED, subject.getKey(), property.getKey())) {
                            array = Json.createArrayBuilder();
                            
                        } else {
                            array = Json.createArrayBuilder(JsonUtils.toJsonArray(result.get(Keywords.MERGED, subject.getKey(), property.getKey())));
                        }
                        
                        JsonUtils.toJsonArray(property.getValue()).forEach(array::add);
                        
                        result.set(Keywords.MERGED, subject.getKey(), property.getKey(), array.build());
                    }
                    
                }
                
            }
        }
        
        if (result.index.get(Keywords.MERGED) != null) {
            index.put(Keywords.MERGED, result.index.get(Keywords.MERGED));
        }
    }

    public boolean contains(String id) {
        return index.containsKey(id);
    }
    
    @Override
    public String toString() {
        return Objects.toString(index);
    }
}