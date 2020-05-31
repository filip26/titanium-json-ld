package com.apicatalog.jsonld.flattening;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.json.JsonUtils;

public final class FlatteningBuilder {

    // required
    private JsonStructure element;
    private final BlankNodeIdGenerator idGenerator;
    
    // optional
    private boolean ordered;
    
    private FlatteningBuilder(final JsonStructure element, final BlankNodeIdGenerator idGenerator) {
        this.element = element;
        this.idGenerator = idGenerator;
        
        // default values
        this.ordered = false;
    }
    
    public static final FlatteningBuilder with(final JsonStructure element, final BlankNodeIdGenerator idGenerator) {
        return new FlatteningBuilder(element, idGenerator);
    }
    
    public FlatteningBuilder ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public JsonArray build() throws JsonLdError {
        
        // 1.
        NodeMap nodeMap = new NodeMap();
        
        // 2.
        NodeMapBuilder.with(element, nodeMap, idGenerator).build();
        
        // 3.
        Map<String, JsonObject> defaultGraph = nodeMap.get(Keywords.DEFAULT);

        // 4.
        for (String graphName : nodeMap.keys(ordered)) {

            if (Keywords.DEFAULT.equals(graphName)) {
                continue;
            }
            
            Map<String, JsonObject> graph = nodeMap.get(graphName);
            
            // 4.1.
            if (!defaultGraph.containsKey(graphName)) {
                defaultGraph.put(graphName, Json.createObjectBuilder().add(Keywords.ID, graphName).build());
            }
            
            // 4.2.
            Map<String, JsonValue> entry = new LinkedHashMap<>(defaultGraph.get(graphName).asJsonObject());
            
            // 4.3.
            JsonArrayBuilder graphArray =  Json.createArrayBuilder();
            
            // 4.4.
            List<String> ids = new ArrayList<>(graph.keySet());
            
            if (ordered) {
                Collections.sort(ids);
            }
            
            for (String id : ids) {
                JsonValue node = graph.get(id);

                if (JsonUtils.isObject(node) && node.asJsonObject().size() == 1 && node.asJsonObject().containsKey(Keywords.ID)) {
                    continue;
                }

                graphArray.add(node);
            }

            entry.put(Keywords.GRAPH, graphArray.build());
 
            defaultGraph.put(graphName, JsonUtils.toJsonObject(entry));
        }
        
        // 5.
        Collection<JsonValue> flattened = new LinkedList<>();
        
        // 6.
        List<String> keys = new ArrayList<>(defaultGraph.keySet());
        
        if (ordered) {
            Collections.sort(keys);
        }

        for (String id : keys) {
            
            JsonValue node = defaultGraph.get(id);
            
            if (JsonUtils.isObject(node) && node.asJsonObject().size() == 1 && node.asJsonObject().containsKey(Keywords.ID)) {
                continue;
            }
            
            flattened.add(node);
        }
        
        // 7.
        return JsonUtils.toJsonArray(flattened);
    }
    
}
