package com.apicatalog.jsonld.context;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public final class InverseContext {

    private final Map<String ,Map<String, Map<String, Map<String, String>>>> context;
    
    public InverseContext() {
        this.context = new LinkedHashMap<>();
    }
    
    private void set(final String variable, final String container, final String type, final String key, final String value) {
        context.computeIfAbsent(variable, x -> new LinkedHashMap<>())
                .computeIfAbsent(container, x -> new LinkedHashMap<>())
                .computeIfAbsent(type, x -> new LinkedHashMap<>())                
                .put(key, value);
    }

    public boolean doesNotContain(final String variable, final String container, final String type) {
        return !context.containsKey(variable)
                || !context.get(variable).containsKey(container)
                || !context.get(variable).get(container).containsKey(type);        
    }
    
    public boolean doesNotContain(final String variable, final String container, final String type, final String key) {
        return doesNotContain(variable, container, type)
                || !context.get(variable).get(container).get(type).containsKey(key);
    }

    public boolean contains(final String variable) {
        return context.containsKey(variable);
    }

    public void setIfAbsent(final String variable, final String container, final String type, final String key, final String value) {
        if (doesNotContain(variable, container, type, key)) {
            set(variable, container, type, key, value);
        }
    }
    
    public Optional<String> get(final String variable, final String container, final String type, final String key) {
        if (doesNotContain(variable, container, type, key)) {
            return Optional.empty();
        }
        return Optional.ofNullable(context.get(variable).get(container).get(type).get(key));
    }
}