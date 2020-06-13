package com.apicatalog.jsonld.framing;

import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdEmbed;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;

public final class Frame {

    private final JsonObject frame;
    
    private Frame(final JsonObject frame) {
        this.frame = frame;
    }
    
    public static final Frame of(JsonStructure structure) throws JsonLdError {
        
        final JsonObject frameObject;;

        // 1.
        if (JsonUtils.isArray(structure)) {

            if (structure.asJsonArray().size() != 1  
                    || JsonUtils.isNotObject(structure.asJsonArray().get(0))
                    ) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
            }

            frameObject = structure.asJsonArray().getJsonObject(0);

            
        } else if (JsonUtils.isObject(structure)) {
            
            frameObject = structure.asJsonObject();

        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
        }
        
        // 1.2.
        if (frameObject.containsKey(Keywords.ID) && !validateFrameId(frameObject)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @id is not valid.");
        }
        
        // 1.3.
        if (frameObject.containsKey(Keywords.TYPE) && !validateFrameType(frameObject)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Fram @type is not valid.");
        }
        return new Frame(frameObject);
    }
    
    
    public JsonLdEmbed getEmbed(JsonLdEmbed defaultValue) throws JsonLdError {
        
        if (frame.containsKey(Keywords.EMBED)) {

            JsonValue embed = frame.get(Keywords.EMBED);

            if (embed == null || JsonUtils.isNull(embed)) {
                return defaultValue;
            }
            
            if (ValueObject.isValueObject(embed)) {
                embed = ValueObject.getValue(embed);
            }
            
            if (JsonUtils.isString(embed)) {

                String stringValue = ((JsonString)embed).getString();
             
//                if ("@last".equals(stringValue)) {
//                    return JsonLdEmbed.ONCE;
//                }
                
                if (Keywords.noneMatch(stringValue, Keywords.ALWAYS, Keywords.ONCE, Keywords.NEVER)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE);
                }
                
                return JsonLdEmbed.valueOf(stringValue.substring(1).toUpperCase());
                
            } else if (JsonUtils.isFalse(embed)) {
                return JsonLdEmbed.NEVER;
                
            } else if (JsonUtils.isTrue(embed)) {
                return JsonLdEmbed.ONCE;
            }

            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE);             
         }
        
        return defaultValue;
    }
    
    public boolean getExplicit(boolean defaultValue) throws JsonLdError {
        return getBoolean(frame, Keywords.EXPLICIT, defaultValue);
    }
    
    public boolean getRequireAll(boolean defaultValue) throws JsonLdError {
        return getBoolean(frame, Keywords.REQUIRE_ALL, defaultValue);
    }
    
    public static final boolean getBoolean(JsonObject frame, String key, boolean defaultValue) throws JsonLdError {
        if (frame.containsKey(key)) {

            JsonValue value = frame.get(key);
            
            if (ValueObject.isValueObject(value)) {
                value = ValueObject.getValue(value);
            }

            if (JsonUtils.isNotBoolean(value)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
            }
      
            return JsonUtils.isTrue(value);
        }
        return defaultValue; 

    }
    
    private static final boolean validateFrameId(JsonObject frame) {
        
        final JsonValue idValue = frame.get(Keywords.ID);
        
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
    
    private static final boolean validateFrameType(JsonObject frame) {

        final JsonValue typeValue = frame.get(Keywords.TYPE);
        
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
        return 
                JsonUtils.isEmptyArray(typeValue)
                || JsonUtils.isString(typeValue) && UriUtils.isAbsoluteUri(((JsonString)typeValue).getString());
    }
    
    public Set<String> keys() {
        return frame.keySet();
    }

    public JsonValue get(String property) {
        return frame.get(property);
    }

    public boolean contains(String property) {
        return frame.containsKey(property);
    }

    public boolean isEmpty() {
        return frame.keySet()
                    .stream()
                    .allMatch(Arrays.asList(
                                        Keywords.DEFAULT,
                                        Keywords.OMIT_DEFAULT, 
                                        Keywords.EMBED, 
                                        Keywords.EXPLICIT, 
                                        Keywords.REQUIRE_ALL
                                        )::contains);
    }

    public boolean isWildCard(String property) {
        return frame.containsKey(property) 
                    && (JsonUtils.isEmptyObject(get(property))
                            || (JsonUtils.isArray(get(property))
                                    && get(property).asJsonArray().size() == 1
                                    && JsonUtils.isEmptyObject(get(property).asJsonArray().get(0)))
                            );
    }

    public boolean isNone(String property) {
        return frame.containsKey(property) 
                    && JsonUtils.isEmptyArray(get(property))
                    ;
    }

    public boolean isNotEmpty(String property) {
        return frame.containsKey(property) 
                            ;
    }

    
    public Collection<JsonValue> getArray(String property) {
        return frame.containsKey(property)
                    ? JsonUtils.toJsonArray(frame.get(property))
                    : JsonValue.EMPTY_JSON_ARRAY
                    ;
    }
    
    @Override
    public String toString() {
        return frame.toString();
    }
}
