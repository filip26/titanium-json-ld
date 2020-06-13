package com.apicatalog.jsonld.framing;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.json.JsonArray;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;

public final class FrameMatcher {

    // required
    private FramingState state;
    private List<String> subjects;
    private Frame frame;
    private boolean requireAll;
    
    private FrameMatcher(FramingState state, List<String> subjects, Frame frame, boolean requireAll) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.requireAll = requireAll; 
    }
    
    public static final FrameMatcher with(FramingState state, List<String> subjects, Frame frame, boolean requireAll) {
        return new FrameMatcher(state, subjects, frame, requireAll);
    }
    
    public List<String> match() throws JsonLdError {
   
        // 1.
        if (frame.isEmpty()) {
            return subjects;
        }
        
        final List<String> result = new ArrayList<>();
        
        for (final String subject : subjects) {
                                    
            if (match(subject)) {
                result.add(subject);
            }
        }
        
        return result;
    }
    
    private boolean match(final String subject) throws JsonLdError {
        
        final Map<String, JsonValue> node = state.getGraphMap().get(state.getGraphName(), subject);
        
        for (final String property : frame.keys()) {

            JsonValue nodeValue = node.get(property);

            // 2.1.
            if (Keywords.ID.equals(property)) {

                nodeValue = JsonUtils.toJsonArray(nodeValue);
                
                if (JsonUtils.toJsonArray(frame.get(property)).stream().anyMatch(nodeValue.asJsonArray()::contains)
                        || (frame.isWildCard(Keywords.TYPE) || frame.isNone(Keywords.NONE))
                        ) {
              
                    if (requireAll) {
                        continue;
                    }
                    return true;                    
                }
                return false;
                
            // 2.2.
            } else if (Keywords.TYPE.equals(property)) {

                if ((JsonUtils.isNotNull(nodeValue) && !nodeValue.asJsonArray().isEmpty() && frame.isWildCard(property))
                        || ((JsonUtils.isNull(nodeValue) || nodeValue.asJsonArray().isEmpty()) && frame.isNone(property))
                        || (JsonUtils.isNotNull(nodeValue) &&frame.getArray(property).stream().anyMatch(nodeValue.asJsonArray()::contains))
                        ){
                    
                    if (requireAll) {
                        continue;
                    }
                    return true;
                }
                return false;

            // skip other keywords
            } else if (Keywords.matchForm(property)) {
                continue;
            }
         
            Frame propertyFrame;
            
            System.out.println("TODO " + property);
            
            // 2.5.
//            if (JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value) /*TODO @default||*/ ) {
//                return true;
//            }
            

            JsonArray nodeValues = JsonUtils.toJsonArray(nodeValue);
            
            // 2.6.
            if (nodeValues.isEmpty() && frame.isNone(property)) {
                
                if (requireAll) {
                    continue;
                }
                return true;                
            }

            // 2.7.
            if (!nodeValues.isEmpty() && frame.isWildCard(property)) {
                if (requireAll) {
                    continue;
                }
                return true;
            }

            return false;
        }

        return true;
    }
    
}
