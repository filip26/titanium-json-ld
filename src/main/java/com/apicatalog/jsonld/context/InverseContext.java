package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;

public final class InverseContext {

    private final Map<String ,Map<String, Map<String, Map<String, String>>>> context;
    
    public InverseContext() {
        this.context = new LinkedHashMap<>();
    }
    
    private void set(String variable, String container, String type, String key, String value) {
        context.computeIfAbsent(variable, x -> new LinkedHashMap<>())
                .computeIfAbsent(container, x -> new LinkedHashMap<>())
                .computeIfAbsent(type, x -> new LinkedHashMap<>())                
                .put(key, value);
    }

    public boolean doesNotContain(String variable, String container, String type) {
        return !context.containsKey(variable)
                || !context.get(variable).containsKey(container)
                || !context.get(variable).get(container).containsKey(type);        
    }
    
    public boolean doesNotContain(String variable, String container, String type, String key) {
        return doesNotContain(variable, container, type)
                || !context.get(variable).get(container).get(type).containsKey(key);
    }

    public boolean contains(String variable) {
        return context.containsKey(variable);
    }

    public void setIfAbsent(String variable, String container, String type, String key, String value) {
        if (doesNotContain(variable, container, type, key)) {
            set(variable, container, type, key, value);
        }
    }
    
    public String get(String variable, String container, String type, String key) {
        if (doesNotContain(variable, container, type, key)) {
            return null;
        }
        return context.get(variable).get(container).get(type).get(key);
    }
}