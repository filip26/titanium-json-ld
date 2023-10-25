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
package no.hasmac.rdf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

final class NodeMapper {

    private final Map<String, NodeCluster> mapping;
    private final List<NodeCluster> clusters;

    private final int permutations;

    private int iterator;

    private NodeMapper() {
        this(-1, null, null);
    }

    private NodeMapper(int permutations, final Map<String, NodeCluster> mapping, List<NodeCluster> clusters) {
        this.mapping = mapping;
        this.clusters = clusters;
        this.permutations = permutations;
        this.iterator = 0;
    }

    public static NodeMapper create(List<RdfNQuad> from, List<RdfNQuad> to) {

        NodeClassifier sourceClassifier = new NodeClassifier();
        to.stream().forEach(sourceClassifier::add);

        NodeClassifier targetClassifier = new NodeClassifier();
        from.stream().forEach(targetClassifier::add);

        // cannot create mapping, blank nodes count do not match
        if (sourceClassifier.size() != targetClassifier.size()) {
            return new NodeMapper();
        }

        Map<NodeCategory, List<String>> reducedSource = sourceClassifier.reduce();
        if (reducedSource == null) {
            return new NodeMapper();
        }

        Map<NodeCategory, List<String>> reducedTarget = targetClassifier.reduce();
        if (reducedTarget == null) {
            return new NodeMapper();
        }

        List<NodeCluster> clusters = new ArrayList<>();

        Map<String, NodeCluster> mapping = merge(reducedSource, reducedTarget, clusters);

        if (mapping == null) {
            return new NodeMapper();
        }

        return new NodeMapper(
                        clusters.stream().map(NodeCluster::permutations).reduce(1, Math::multiplyExact),
                        mapping,
                        clusters
                        );
    }

    private static Map<String, NodeCluster> merge(Map<NodeCategory, List<String>> reducedSource, Map<NodeCategory, List<String>> reducedTarget, List<NodeCluster> clusters) {

        if (reducedSource.size() != reducedTarget.size()) {
            return null;
        }

        Map<String, NodeCluster> mapping = new HashMap<>();

        for (Entry<NodeCategory, List<String>> source : reducedSource.entrySet()) {

            List<String> target = reducedTarget.get(source.getKey());

            if (target == null || target.size() != source.getValue().size()) {
                return null;
            }

            final NodeCluster cluster = NodeCluster.create(source.getValue(), target);
            if (cluster == null) {
                return null;
            }
            clusters.add(cluster);
            source.getValue().forEach(l -> mapping.put(l, cluster));
        }

        return mapping;
    }

    public boolean hasNext() {
        return iterator != -1 && iterator < permutations;
    }

    public Map<String, String> next() {

        if (!hasNext() || clusters == null) {
            throw new IllegalArgumentException();
        }


        if (iterator > 0) {
            boolean overflow = true;

            for (NodeCluster cluster : clusters) {
                if (overflow) {
                    overflow = cluster.next();
                }
                if (!overflow) {
                    break;
                }
            }
        }

        Map<String, String> result = new HashMap<>(mapping.size());

        for (Map.Entry<String, NodeCluster> entry : mapping.entrySet()) {
            result.put(entry.getKey(), entry.getValue().mapping(entry.getKey()));
        }

        iterator++;

        return result;
    }

    public int permutations() {
        return permutations;
    }

}
