package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

final class NodeMapper {

    private final Map<String, NodeCluster> clusters;

    private final int permutations;
    
    private int iterator;
    
    private NodeMapper(int permutations, final Map<String, NodeCluster> clusters) {
        this.clusters = clusters;
        this.permutations = permutations;
        this.iterator = 0;
    }
    
    public static final NodeMapper create(List<RdfTriple> from, List<RdfTriple> to) {
        
        NodeClassifier sourceClassifier = new NodeClassifier();
        to.stream().forEach(sourceClassifier::add);
        
        NodeClassifier targetClassifier = new NodeClassifier();
        from.stream().forEach(targetClassifier::add);
        
        // cannot create mapping, blank nodes count do not match
        if (sourceClassifier.size() != targetClassifier.size()) {
            return new NodeMapper(-1, null);
        }
        
        Map<NodeCategory, List<String>> reducedSource = sourceClassifier.reduce();
        if (reducedSource == null) {
            return new NodeMapper(-1, null);
        }
        
        Map<NodeCategory, List<String>> reducedTarget = targetClassifier.reduce();
        if (reducedTarget == null) {
            return new NodeMapper(-1, null);
        }
        
        Map<String, NodeCluster> clusters = merge(reducedSource, reducedTarget);
        
        if (clusters == null) {
            return new NodeMapper(-1, null);
        }
        
        return new NodeMapper(
                        clusters.values().stream().map(NodeCluster::permutations).reduce(1, Math::multiplyExact),
                        clusters
                        );
    }
    
    private final static Map<String, NodeCluster> merge(Map<NodeCategory, List<String>> reducedSource, Map<NodeCategory, List<String>> reducedTarget) {
        
        if (reducedSource.size() != reducedTarget.size()) {
            return null;
        }
        
        Map<String, NodeCluster> clusters = new HashMap<>();
        
        for (Entry<NodeCategory, List<String>> source : reducedSource.entrySet()) {

            List<String> target = reducedTarget.get(source.getKey());
            
            if (target == null || target.size() != source.getValue().size()) {
                return null;
            }

            final NodeCluster cluster = NodeCluster.create(source.getValue(), target);
            if (cluster == null) {
                return null;
            }
            
            source.getValue().forEach(l -> clusters.put(l, cluster));
        }
        
        return clusters;
    }

    public boolean hasNext() {
        return iterator != -1 && iterator < permutations;
    }

    public Map<String, String> next() {
        
        if (!hasNext() || clusters == null) {
            throw new IllegalArgumentException();
        }

        boolean overflow = true;
        
        for (NodeCluster cluster : clusters.values()) {
            if (overflow) {
                overflow = cluster.next(); 
            }
            if (!overflow) {
                break;
            }
        }
        
        Map<String, String> mapping = new HashMap<>(clusters.size());
        
        for (Map.Entry<String, NodeCluster> entry : clusters.entrySet()) {
            mapping.put(entry.getKey(), entry.getValue().mapping(entry.getKey()));
        }
        
        iterator++;
        
        return mapping;
    }    
    
}
