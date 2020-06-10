package com.apicatalog.jsonld.framing;

import java.util.ArrayList;
import java.util.List;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

public final class FrameMatcher {

    // required
    private FramingState state;
    private List<String> subjects;
    private JsonObject frame;
    private boolean requireAll;
    
    private FrameMatcher(FramingState state, List<String> subjects, JsonObject frame, boolean requireAll) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.requireAll = requireAll; 
    }
    
    public static final FrameMatcher with(FramingState state, List<String> subjects, JsonObject frame, boolean requireAll) {
        return new FrameMatcher(state, subjects, frame, requireAll);
    }
    
    public List<String> match() throws JsonLdError {
        
        // 1.
        if (frame.isEmpty()) {
            return subjects;
        }
        
        List<String> result = new ArrayList<>();
        
        for (final String node : subjects) {
            
            if (match(node)) {
                result.add(node);
            }
        }
        
        return result;
    }
    
    private boolean match(final String property) throws JsonLdError {
        
        JsonArray value = frame.getJsonArray(property);
        
        // 2.1.
        if (Keywords.ID.equals(property)) {
            //TODO
            
            return false;
            
        // 2.2.
        } else if (Keywords.TYPE.equals(property)) {
          //TODO
            
            return false;
        }
        
        // 2.5.
        if (JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value) /*TODO @default||*/ ) {
            return true;
        }
        
        // 2.6.
        
        
        return false;
    }
    
}
