package com.apicatalog.jsonld.framing;

import java.util.Collections;
import java.util.List;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;

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
    
    // runtime
    private JsonObject frameObject;
    
    private FramingBuilder(FramingState state, List<String> subjects, JsonArray frame, List<JsonValue> parent, String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
        
        this.frameObject = null;
    }
    
    public static final FramingBuilder with(FramingState state, List<String> subjects, JsonArray frame, List<JsonValue> parent, String activeProperty) {
        return new FramingBuilder(state, subjects, frame, parent, activeProperty);
    }
    
    public void build() throws JsonLdError {
        
        // 1.
        if (JsonUtils.isArray(frame)) {
            
            if (frame.asJsonArray().size() > 1 
                    || frame.asJsonArray().isEmpty() 
                    || JsonUtils.isNotObject(frame.asJsonArray().get(0))
                    ) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
            }
            
            frameObject = frame.asJsonArray().getJsonObject(0); 
            
        } else if (JsonUtils.isObject(frame)) {
            
            frameObject = frame.asJsonObject();
            
        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
        }
        
        // 1.2.
        if (frameObject.containsKey(Keywords.ID) && !validateFrameId()) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
        }
        
        // 1.3.
        if (frameObject.containsKey(Keywords.TYPE) && !validateFrameType()) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
        }
        
        // 2.
        //TODO
        boolean embed = false;
        boolean explicit = false;
        boolean requireAll = false;
        System.out.println(": " + subjects);
        // 3.
        final List<String> matchedSubjects = FrameMatcher.with(state, subjects, frameObject, requireAll).match();
        
        // 4.
        if (ordered) {
            Collections.sort(matchedSubjects);
        }

        for (final String id : matchedSubjects) {
            
            System.out.println(": " + id);
            
        }
        
        
    }
    
    private boolean validateFrameId() {
        
        final JsonValue idValue = frameObject.get(Keywords.ID);
        
        if (JsonUtils.isArray(idValue) && JsonUtils.isNotEmptyArray(idValue)) {
            
            if (idValue.asJsonArray().size() == 1
                   && JsonUtils.isEmptyObject(idValue.asJsonArray().get(0))) {
                return true;
            } 
            
            for (final JsonValue item : idValue.asJsonArray()) {
                if (JsonUtils.isNotString(item) || UriUtils.isNotAbsoluteUri(((JsonString)item).getString())) {
                    return false;
                }
            }
            return true;
            
        } 
        return JsonUtils.isString(idValue) && UriUtils.isAbsoluteUri(((JsonString)idValue).getString());
    }
    
    private boolean validateFrameType() {
        
        final JsonValue typeValue = frameObject.get(Keywords.TYPE);
        
        if (JsonUtils.isArray(typeValue) && JsonUtils.isNotEmptyArray(typeValue)) {
                        
            if (typeValue.asJsonArray().size() == 1
                   && (JsonUtils.isEmptyObject(typeValue.asJsonArray().get(0))
                           || (JsonUtils.isObject(typeValue) 
                                   && typeValue.asJsonObject().containsKey(Keywords.DEFAULT)
                                   )
                    )) {
                return true;
            } 
            
            for (final JsonValue item : typeValue.asJsonArray()) {
                if (JsonUtils.isNotString(item) || UriUtils.isNotAbsoluteUri(((JsonString)item).getString())) {
                    return false;
                }
            }
            return true;
            
        } 
        return JsonUtils.isString(typeValue) && UriUtils.isAbsoluteUri(((JsonString)typeValue).getString());
    }
}
