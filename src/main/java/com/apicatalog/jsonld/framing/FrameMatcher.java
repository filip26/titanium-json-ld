package com.apicatalog.jsonld.framing;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.json.JsonObject;
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



        
        Map<String, JsonValue> node = state.getGraphMap().get(state.getGraphName(), subject);
        
        int match = 0;
        int count = 0;
        
        for (final String property : frame.keys()) {

//            if (!requireAll && match > 0) {
//                return true;
//            }

            JsonValue nodeValue = node.get(property);


            // 2.1.
            if (Keywords.ID.equals(property)) {

                
                //TODO
                
                return false;
                
            // 2.2.
            } else if (Keywords.TYPE.equals(property)) {

                if (nodeValue != null) {
                    if (frame.getArray(property).stream().anyMatch(nodeValue.asJsonArray()::contains)) {
                        match++;
                        return true;
                        
                    } else if (!nodeValue.asJsonArray().isEmpty() && frame.isWildCard(property)) {
                        match++;
                        return true;
                    }
                }

                return false;

            } else if (Keywords.matchForm(property)) {
                continue;
            }
            count ++;

            
            // 2.5.
//            if (JsonUtils.isNull(value) || JsonUtils.isEmptyArray(value) /*TODO @default||*/ ) {
//                return true;
//            }
            
            // 2.6.
            
            

        }
       // return !requireAll && match > 0 || requireAll && match == frame.size();
        return count == 0;
    }
    
}
