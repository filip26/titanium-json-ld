package com.apicatalog.jsonld.http.media;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public final class MediaTypeParameters {

    protected static final MediaTypeParameters EMPTY = new MediaTypeParameters(Collections.emptyMap());
    
    private final Map<String, List<String>> parameters;
    
    protected MediaTypeParameters(final Map<String, List<String>> parameters) {
        this.parameters = parameters;
    }
    
    public Set<String> names() {
        return parameters.keySet();
    }
    
    public List<String> values(final String name) {
        return parameters.containsKey(name)
                    ? Collections.unmodifiableList(parameters.get(name))
                    : Collections.emptyList();
    }
    
    public Optional<String> firstValue(final String name) {
        return parameters.containsKey(name)
                    ? Optional.of(parameters.get(name).get(0))
                    : Optional.empty();
    }
    
    public boolean isEmpty() {
        return parameters.isEmpty();
    }
}
