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

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Utils;

public final class Flattening {

    // required
    private JsonStructure element;
    
    // optional
    private boolean ordered;
    
    private Flattening(final JsonStructure element) {
        this.element = element;
        
        // default values
        this.ordered = false;
    }
    
    public static final Flattening with(final JsonStructure element) {
        return new Flattening(element);
    }
    
    public Flattening ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public JsonArray flatten() throws JsonLdError {
        
        // 1.
        NodeMap nodeMap = new NodeMap();
        
        // 2.
        NodeMapBuilder.with(element, nodeMap).build();
        
        // 3.
        Map<String, JsonObject> defaultGraph = nodeMap.get(Keywords.DEFAULT);

        // 4.
        for (String graphName : Utils.index(nodeMap.graphs(), ordered)) {

            if (Keywords.DEFAULT.equals(graphName)) {
                continue;
            }
            
            Map<String, JsonObject> graph = nodeMap.get(graphName);
            
            // 4.1.
            if (!defaultGraph.containsKey(graphName)) {
                defaultGraph.put(graphName, Json.createObjectBuilder().add(Keywords.ID, graphName).build());
            }
            
            // 4.2.
            final JsonObjectBuilder entry = Json.createObjectBuilder(defaultGraph.get(graphName).asJsonObject());
            
            // 4.3.
            final JsonArrayBuilder graphArray =  Json.createArrayBuilder();
            
            // 4.4.
            for (final String id : Utils.index(graph.keySet(), ordered)) {
                
                final JsonValue node = graph.get(id);

                if (JsonUtils.isObject(node) && node.asJsonObject().size() == 1 && node.asJsonObject().containsKey(Keywords.ID)) {
                    continue;
                }

                graphArray.add(node);
            }

            entry.add(Keywords.GRAPH, graphArray.build());
 
            defaultGraph.put(graphName, entry.build());
        }
        
        // 5.
        final Collection<JsonValue> flattened = new LinkedList<>();
        
        // 6.
        for (String id : Utils.index(defaultGraph.keySet(), ordered)) {
            
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
