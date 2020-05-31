package com.apicatalog.jsonld.flattening;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#generate-blank-node-identifier">Generate Blank Node Identifier</a>
 */
public final class BlankNodeIdGenerator {

    private final Map<String, String> map;
    
    private Integer counter;
    
    public BlankNodeIdGenerator() {
        this.map = new HashMap<>();
        this.counter = 0;
    }

    public String createIdentifier() {
        return "_:b".concat(Integer.toString(counter++));
    }

    public String createIdentifier(String identifier) {
        
        if (identifier == null || identifier.isBlank()) {
            return createIdentifier();            
        }
        
        if (map.containsKey(identifier)) {
            return map.get(identifier);
        }
        
        final String blankId = createIdentifier();
        
        map.put(identifier, blankId);
        
        return blankId;
    }

}
