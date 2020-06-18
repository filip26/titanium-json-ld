package com.apicatalog.jsonld.framing;

import java.util.Arrays;

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

        return (JsonUtils.isNotNull(value1) && isWildcard(value2))
                    || (JsonUtils.isNotNull(value2) && JsonUtils.toJsonArray(value2).contains(value1))
                    ;
    }
    
    private boolean matchType(JsonValue type2) {
        
        final JsonValue type1 = value.containsKey(Keywords.TYPE) 
                ? value.get(Keywords.TYPE)
                : null;

        return (JsonUtils.isNotNull(type1) && isWildcard(type2))
                    || (JsonUtils.isNull(type1) && isNone(type2))
                    || (JsonUtils.isNotNull(type2) && JsonUtils.toJsonArray(type2).contains(type1))
                ;
    }
    
    private boolean matchLanguage(JsonValue lang2) {

        final String lang1 = value.containsKey(Keywords.LANGUAGE) 
                                    ? value.getString(Keywords.LANGUAGE).toLowerCase()
                                    : null;
                                
        if ((lang1 != null && isWildcard(lang2))
                || (lang1 == null && isNone(lang2))
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
                                .anyMatch(x -> x.equalsIgnoreCase(lang1));
    }
    
    protected static final boolean isWildcard(JsonValue value) {
        
        if (JsonUtils.isEmptyObject(value)) {
            return true;
        }
        
        JsonObject frame = null;
        
        if (JsonUtils.isObject(value)) {
            
            frame = (JsonObject)value;
            
        } else if (JsonUtils.isArray(value) 
                    && value.asJsonArray().size() == 1
                    && JsonUtils.isObject(value.asJsonArray().get(0))) {
            
            frame = value.asJsonArray().getJsonObject(0);
        }

        if (frame == null) {
            return false;
        }

        return frame.isEmpty() || frame.keySet()
                .stream()
                .allMatch(Arrays.asList(
                            Keywords.DEFAULT,
                            Keywords.OMIT_DEFAULT, 
                            Keywords.EMBED, 
                            Keywords.EXPLICIT, 
                            Keywords.REQUIRE_ALL
                            )::contains);
    }
    
    private static final boolean isNone(JsonValue value) {
        return JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value);
    }
}
