package com.apicatalog.jsonld.flattening;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;

import javax.json.JsonArray;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.json.JsonUtils;

public final class FlatteningBuilder {

    // required
    private JsonStructure element;
    
    // optional
    private boolean ordered;
    
    private FlatteningBuilder(final JsonStructure element) {
        this.element = element;
        
        // default values
        this.ordered = false;
    }
    
    public static final FlatteningBuilder with(final JsonStructure element) {
        return new FlatteningBuilder(element);
    }
    
    public FlatteningBuilder ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public JsonArray build() throws JsonLdError {
        
        // 1.
        Map<String, JsonValue> nodeMap = new LinkedHashMap<>();
        nodeMap.put(Keywords.DEFAULT, JsonValue.EMPTY_JSON_OBJECT);
        
        // 2.
        NodeMapBuilder.with(element, nodeMap).build();
        
        // 3.
        //TODO
        
        // 4.
        //TODO
        
        // 5.
        Collection<JsonValue> flattened = new LinkedList<>();
        
        // 6.
        //TODO
        
        // 7.
        return JsonUtils.toJsonArray(flattened);
    }
    
}
