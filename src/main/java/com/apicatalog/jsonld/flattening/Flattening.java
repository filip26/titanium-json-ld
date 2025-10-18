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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Stream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.lang.Keywords;

public final class Flattening {

    public static Object flatten(final Collection<?> elements, boolean ordered) throws JsonLdError {

        // 1.
        final var nodeMap = new NodeMap();

        // 2.
        NodeMapBuilder.with(elements, nodeMap).build();

        // 3.
        final var defaultGraph = nodeMap.get(Keywords.DEFAULT).orElseThrow(IllegalStateException::new);

        // 4.
        final var namedGraphs = (ordered
                ? nodeMap.graphs().stream().sorted()
                : nodeMap.graphs().stream())
                .filter(Predicate.not(Keywords.DEFAULT::equals))
                .iterator();

        while (namedGraphs.hasNext()) {

            final var graphName = namedGraphs.next();

            final var graph = nodeMap.get(graphName).orElseThrow(IllegalStateException::new);

            // 4.1.
            if (!defaultGraph.containsKey(graphName)) {
                defaultGraph.put(graphName, Map.of(Keywords.ID, graphName));
            }

            // 4.2.
            final var entry = new LinkedHashMap<String, Object>(defaultGraph.get(graphName));

            entry.put(Keywords.GRAPH, filter(graph, ordered).toList());

            defaultGraph.put(graphName, entry);
        }

        return filter(defaultGraph, ordered).toList();
    }

    private static Stream<?> filter(Map<String, Map<String, Object>> nodeMap, boolean ordered) {
        return (ordered
                ? nodeMap.keySet().stream().sorted()
                : nodeMap.keySet().stream())
                .map(nodeMap::get)
                .filter(node -> node != null
                        && (node.size() != 1 || !node.containsKey(Keywords.ID)));
    }
}
