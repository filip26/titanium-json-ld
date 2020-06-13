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
        return pattern.isEmpty() || matchValue() && matchType() && matchLanguage();
    }
    
    private boolean matchValue() {

        final JsonValue value1 = value.containsKey(Keywords.VALUE) 
                                    ? value.get(Keywords.VALUE)
                                    : null;

        final JsonValue value2 = pattern.containsKey(Keywords.VALUE)
                                    ? pattern.get(Keywords.VALUE)
                                    : null;

                                    
        return (JsonUtils.isNotNull(value1) && JsonUtils.isEmptyObject(value2))
                    || (JsonUtils.isNotNull(value2)  && JsonUtils.toJsonArray(value1).contains(value1))
                    ;
    }
    
    private boolean matchType() {
        
        final JsonValue type1 = value.containsKey(Keywords.TYPE) 
                ? value.get(Keywords.TYPE)
                : null;

        final JsonValue type2 = pattern.containsKey(Keywords.TYPE)
                ? pattern.get(Keywords.TYPE)
                : null;

        return type1 == null && type2 == null 
                || (JsonUtils.isNotNull(type1) && (JsonUtils.isNull(type2) || JsonUtils.isEmptyObject(type2)))
                    || (JsonUtils.isNull(type1) && (JsonUtils.isNull(type2) || JsonUtils.isEmptyArray(type2)))
                    || (JsonUtils.isNotNull(type2) && JsonUtils.toJsonArray(type2).contains(type1))
                ;
    }
    
    private boolean matchLanguage() {
        
        final String lang1 = value.containsKey(Keywords.LANGUAGE) 
                ? value.getString(Keywords.LANGUAGE).toLowerCase()
                : null;

        final JsonValue lang2 = pattern.containsKey(Keywords.LANGUAGE)
                ? pattern.get(Keywords.LANGUAGE)
                : null;

                
                
        if (lang1 == null && lang2 == null || ((lang1 != null && (JsonUtils.isNull(lang2) || JsonUtils.isEmptyObject(lang2)))
                || (lang1 != null && (JsonUtils.isNull(lang2) || JsonUtils.isEmptyArray(lang2)))
                )) {
            
            return true;
        }

        return JsonUtils.isNotNull(lang2) 
                    && JsonUtils.toJsonArray(lang2)
                                .stream()
                                .map(JsonString.class::cast)
                                .map(JsonString::getString)
                                .anyMatch(x -> x.equals(lang1));
    }
}
