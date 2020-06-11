package com.apicatalog.jsonld.framing;

import javax.json.JsonObject;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-framing/#value-matching">Value Pattern Matching Algorithm</a>
 *
 */
public final class ValuePatternMatcher {

    // required
    private JsonObject pattern;
    private JsonObject value;
    
    private ValuePatternMatcher(final JsonObject pattern, final JsonObject value) {
        this.pattern = pattern;
        this.value = value;
    }
    
    public static final  ValuePatternMatcher with(final JsonObject pattern, final JsonObject value) {
        return new ValuePatternMatcher(pattern, value);
    }
    
    public boolean match() {
        
        // if pattern is wildcard
        if (pattern.isEmpty()) {
            return true;
        }
        
        
        return false;
        
    }
}
