package com.apicatalog.jsonld.json;

import javax.json.JsonValue;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {

    private final JsonValue value;
    
    public JsonCanonicalizer(final JsonValue value) {
        this.value = value;
    }
    
    public String canonicalize() {
        
        if (JsonUtils.isNull(value)) {
            
        }
        
        //TODO
        return value.toString();
    }
    
}
