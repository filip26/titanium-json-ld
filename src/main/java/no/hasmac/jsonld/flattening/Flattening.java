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
package no.hasmac.jsonld.flattening;

import java.util.LinkedHashMap;
import java.util.Map;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.Utils;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

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

    public static Flattening with(final JsonStructure element) {
        return new Flattening(element);
    }

    public Flattening ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public JsonArray flatten() throws JsonLdError {

        // 1.
        final NodeMap nodeMap = new NodeMap();

        // 2.
        NodeMapBuilder.with(element, nodeMap).build();

        // 3.
        final Map<String, Map<String, JsonValue>> defaultGraph = nodeMap.get(Keywords.DEFAULT).orElseThrow(IllegalStateException::new);

        // 4.
        for (String graphName : Utils.index(nodeMap.graphs(), ordered)) {

            if (Keywords.DEFAULT.equals(graphName)) {
                continue;
            }

            final Map<String, Map<String, JsonValue>> graph = nodeMap.get(graphName).orElseThrow(IllegalStateException::new);

            // 4.1.
            if (!defaultGraph.containsKey(graphName)) {
                defaultGraph.put(graphName, JsonProvider.instance().createObjectBuilder().add(Keywords.ID, graphName).build());
            }

            // 4.2.
            final Map<String, JsonValue> entry = new LinkedHashMap<>(defaultGraph.get(graphName));

            // 4.3.
            final JsonArrayBuilder graphArray =  JsonProvider.instance().createArrayBuilder();

            // 4.4.
            for (final String id : Utils.index(graph.keySet(), ordered)) {

                final Map<String, JsonValue> node = graph.get(id);

                if (node == null || node.size() == 1 && node.containsKey(Keywords.ID)) {
                    continue;
                }

                graphArray.add(JsonUtils.toJsonObject(node));
            }

            entry.put(Keywords.GRAPH, graphArray.build());

            defaultGraph.put(graphName, entry);
        }

        // 5.
        final JsonArrayBuilder flattened = JsonProvider.instance().createArrayBuilder();

        // 6.
        for (String id : Utils.index(defaultGraph.keySet(), ordered)) {

            final Map<String, JsonValue> node = defaultGraph.get(id);

            if (node == null || node.size() == 1 && node.containsKey(Keywords.ID)) {
                continue;
            }

            flattened.add(JsonUtils.toJsonObject(node));
        }

        // 7.
        return flattened.build();
    }

}
