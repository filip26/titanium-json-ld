package com.apicatalog.rdf;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

final class NodeClassifier {

    private final Map<String, NodeCategory> categories = new HashMap<>();

    protected NodeClassifier() {
    }
        
    protected final void add(RdfNQuad nquad) {
    
        if (nquad.getSubject().isBlankNode()) {
            addSubject(nquad.getSubject().toString(), nquad.getObject().isIRI() ? nquad.getObject().toString() : null);
        } 
        if (nquad.getObject().isBlankNode()) {
            addObject(nquad.getObject().toString(), nquad.getSubject().isIRI() ? nquad.getSubject().toString() : null);
        }                    
        if (nquad.getGraphName().filter(RdfValue::isBlankNode).isPresent()) {
            addGraph(nquad.getGraphName().get().toString());
        }
    }
    
    private final void addSubject(String label, String object) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addSubject(object);
    }
    
    private final void addObject(String label, String subject) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addObject(subject);
    }
    
    private final void addGraph(String label) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addGraph();
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
