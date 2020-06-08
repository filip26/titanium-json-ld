package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;

public final class InverseContext {

    private final Map<String ,Map<String, Map<String, Map<String, String>>>> context;
    
    public InverseContext() {
        this.context = new LinkedHashMap<>();
    }
    
    public boolean contains(String variable) {
        return context.containsKey(variable);
    }

    public Map<String, Map<String, Map<String, String>>> getValue(String variable) {
        return context.get(variable);
    }

    public void add(String variable, Map<String, Map<String, Map<String, String>>> containerMap) {
        context.put(variable, containerMap);
    }
}