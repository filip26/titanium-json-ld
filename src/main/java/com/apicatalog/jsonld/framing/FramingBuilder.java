package com.apicatalog.jsonld.framing;

import java.util.List;

import javax.json.JsonArray;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-framing/#framing-algorithm">Framing Algorithm</a>
 *
 */
public final class FramingBuilder {

    // required
    private FramingState state;
    private List<String> subjects;
    private JsonArray frame;
    private List<JsonValue> parent;
    private String activeProperty;
    
    // optional
    private boolean ordered;
    
    private FramingBuilder(FramingState state, List<String> subjects, JsonArray frame, List<JsonValue> parent, String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
    }
    
    public static final FramingBuilder with(FramingState state, List<String> subjects, JsonArray frame, List<JsonValue> parent, String activeProperty) {
        return new FramingBuilder(state, subjects, frame, parent, activeProperty);
    }
    
    public void build() throws JsonLdError {
        
        // 1.
        if (JsonUtils.isArray(frame)) {
            //TODO
        }
        
        
    }
}
