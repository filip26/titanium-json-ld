package com.apicatalog.jsonld.rdf;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

final class NodeClassifier {

    Map<String, NodeCategory> categories = new HashMap<>();

    protected NodeClassifier() {
    }
        
    protected final void add(RdfTriple triple) {
    
        if (triple.getSubject().isBlankNode()) {
            addSubject(triple.getSubject().asBlankNode().getLabel());
        } 
        if (triple.getObject().isBlankNode()) {
            addObject(triple.getObject().asBlankNode().getLabel());
        }                    
    }
    
    private final void addSubject(String label) {
        NodeCategory group = categories.get(label);
        
        if (group == null) {
            group = new NodeCategory();
            categories.put(label, group);
        }
        group.addSubject();
    }
    
    private final void addObject(String label) {
        NodeCategory cluster = categories.get(label);
        
        if (cluster == null) {
            cluster = new NodeCategory();
            categories.put(label, cluster);
        }
        cluster.addObject();
    }    
    
    public int size() {
        return categories.size();
    }

    public Map<NodeCategory, List<String>> reduce() {
        
        Map<NodeCategory, List<String>> reduced = new HashMap<>();
        
        for (Entry<String, NodeCategory> entry : categories.entrySet()) {
            
            List<String> list = reduced.get(entry.getValue());
            
            if (list == null) {
                list = new LinkedList<>();
                reduced.put(entry.getValue(), list);
            }
            
            list.add(entry.getKey());
        }
        return reduced;
    }
}
