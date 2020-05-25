package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;

public class InverseContext {

    //TODO get rid of all maps
    Map<String, Map<String, Map<String, String>>> context;
    
    public InverseContext() {
        this.context = new LinkedHashMap<>();
    }
    
    public boolean contains(String variable) {
        return context.containsKey(variable);
    }

    public Map getValue(String variable) {
        return context.get(variable);
    }

    public void add(String variable, Map containerMap) {
        context.put(variable, containerMap);
    }

}
