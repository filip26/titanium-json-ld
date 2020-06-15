package com.apicatalog.jsonld.framing;

import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

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
        
        final JsonValue value2 = pattern.containsKey(Keywords.VALUE)
                ? pattern.get(Keywords.VALUE)
                : null;
        
        final JsonValue type2 = pattern.containsKey(Keywords.TYPE)
                ? pattern.get(Keywords.TYPE)
                : null;

        
        final JsonValue lang2 = pattern.containsKey(Keywords.LANGUAGE)
                ? pattern.get(Keywords.LANGUAGE)
                : null;

        if (value2 == null && type2 == null && lang2 == null) {
            return true;
        }
                
        return matchValue(value2) && matchType(type2) && matchLanguage(lang2);
    }
    
    private boolean matchValue(JsonValue value2) {

        final JsonValue value1 = value.containsKey(Keywords.VALUE) 
                                    ? value.get(Keywords.VALUE)
                                    : null;


  
        return (JsonUtils.isNotNull(value1) && JsonUtils.isEmptyObject(value2))
                    || (JsonUtils.isNotNull(value2)  && JsonUtils.toJsonArray(value2).contains(value1))
                    ;
    }
    
    private boolean matchType(JsonValue type2) {
        
        final JsonValue type1 = value.containsKey(Keywords.TYPE) 
                ? value.get(Keywords.TYPE)
                : null;

        return (JsonUtils.isNotNull(type1) && JsonUtils.isNotNull(type2) && JsonUtils.isEmptyObject(type2))
                    || (JsonUtils.isNull(type1) && (JsonUtils.isNull(type2) || JsonUtils.isEmptyArray(type2)))
                    || (JsonUtils.isNotNull(type2) && JsonUtils.toJsonArray(type2).contains(type1))
                ;
    }
    
    private boolean matchLanguage(JsonValue lang2) {
        
        final String lang1 = value.containsKey(Keywords.LANGUAGE) 
                ? value.getString(Keywords.LANGUAGE).toLowerCase()
                : null;
                
                
        if ((lang1 != null && JsonUtils.isNotNull(lang2) && JsonUtils.isEmptyObject(lang2))
                || (lang1 == null && (JsonUtils.isNull(lang2) || JsonUtils.isEmptyArray(lang2)))
                ) {
            
            return true;
        }

        if (lang1 == null || lang2 == null) {
            return false;
        }

        return JsonUtils.isNotNull(lang2) 
                    && JsonUtils.toJsonArray(lang2)
                                .stream()
                                .map(JsonString.class::cast)
                                .map(JsonString::getString)
                                .map(String::toLowerCase)
                                .anyMatch(x -> x.equals(lang1));
    }
}
