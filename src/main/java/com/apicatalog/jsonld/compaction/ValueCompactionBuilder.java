package com.apicatalog.jsonld.compaction;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.DirectionType;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.json.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-compaction">Value Compaction</a>
 *
 */
public final class ValueCompactionBuilder {

    // mandatory
    private ActiveContext activeContext;
    private String activeProperty;
    private JsonObject value;
    
    ValueCompactionBuilder(ActiveContext activeContext, String activeProperty, JsonObject value) {
        this.activeContext = activeContext;
        this.activeProperty = activeProperty;
        this.value = value;
    }
    
    public static ValueCompactionBuilder with(ActiveContext activeContext, String activeProperty, JsonObject value) {
        return new ValueCompactionBuilder(activeContext, activeProperty, value);
    }
    
    public JsonValue build() throws JsonLdError {

        // 1.
        JsonValue result = value;
        
        // 2.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }
        
        // 3.
        //TODO ?!? InverseContext inverseContext = activeContext.getInverseContext();
     
        TermDefinition activePropertyDefinition = activeContext.getTerm(activeProperty);
        
        // 4. - 5.
        JsonValue language = null; 
        DirectionType direction = null;

        if (activePropertyDefinition != null) {
            language = activePropertyDefinition.getLanguageMapping(); 
            direction = activePropertyDefinition.getDirectionMapping();
        }

        if (language == null) {
            language = activeContext.getDefaultLanguage() != null
                            ? Json.createValue(activeContext.getDefaultLanguage())
                            : null;
        }

        if (direction == null) {
            direction = activeContext.getDefaultBaseDirection() != null
                    ? activeContext.getDefaultBaseDirection()
                    : null;            
        }
 
        // 6.
        if (value.containsKey(Keywords.ID) && 
                ((value.size() == 1)
                        || (value.size() == 2 && value.containsKey(Keywords.INDEX))) 
                ) {
            
            // 6.1.
            if (activePropertyDefinition != null && Keywords.ID.equals(activePropertyDefinition.getTypeMapping())) {
                result = JsonUtils.toJsonValue(activeContext
                                                .compactUri(value.getString(Keywords.ID))
                                                .build());

            // 6.2.
            } else if (activePropertyDefinition != null && Keywords.VOCAB.equals(activePropertyDefinition.getTypeMapping())) {
                result = JsonUtils.toJsonValue(activeContext
                                                .compactUri(value.getString(Keywords.ID))
                                                .vocab(true)
                                                .build());                
            }
        // 7.
        } else if (value.containsKey(Keywords.TYPE)
                    && activePropertyDefinition != null
                    && JsonUtils.contains(
                                    activePropertyDefinition.getTypeMapping(),
                                    value.get(Keywords.TYPE)
                                        )
                    ) {

            result = value.get(Keywords.VALUE);
        
        // 8.
        } else if (activePropertyDefinition != null && Keywords.NONE.equals(activePropertyDefinition.getTypeMapping())
                    || (value.containsKey(Keywords.TYPE)
                            && (activePropertyDefinition == null
                                    || !JsonUtils.contains(
                                            activePropertyDefinition.getTypeMapping(),
                                            value.get(Keywords.TYPE)
                                            )
                                    ))
                ) {
            
            // 8.1.
            JsonArrayBuilder types = Json.createArrayBuilder();
            
            JsonValue resultTypes = result.asJsonObject().get(Keywords.TYPE);
            
            if (JsonUtils.isNotNull(resultTypes)) {
                for (JsonValue type : JsonUtils.toJsonArray(resultTypes)) {
    
                    types.add(activeContext.compactUri(((JsonString)type).getString()).vocab(true).build());                    
                }
                
                Map<String, JsonValue> resultMap = new LinkedHashMap<>(result.asJsonObject());
                resultMap.put(Keywords.TYPE, types.build());
                
                result = JsonUtils.toJsonObject(resultMap);
            }
            
        // 9.
        } else if (JsonUtils.isNotString(value.get(Keywords.VALUE))) {
            
            if (!value.containsKey(Keywords.INDEX) 
                    || (activePropertyDefinition != null
                            && activePropertyDefinition.hasContainerMapping(Keywords.INDEX)
                    )
                ) {
                result = value.get(Keywords.VALUE);
            }

        // 10.
        } else if ((value.containsKey(Keywords.LANGUAGE)
                                && JsonUtils.isString(value.get(Keywords.LANGUAGE))
                                && JsonUtils.isString(language)
                                && (((JsonString)language).getString().equalsIgnoreCase(value.getString(Keywords.LANGUAGE)))
                                )
                        || (JsonUtils.isNull(language)
                                && direction != null && direction != DirectionType.NULL
                                && value.containsKey(Keywords.DIRECTION)
                                && JsonUtils.isString(value.get(Keywords.DIRECTION))
                                && direction == DirectionType.valueOf(value.getString(Keywords.DIRECTION).toUpperCase())
                                )
                        || ((direction == null || direction == DirectionType.NULL)
                                && value.containsKey(Keywords.DIRECTION)
                                )
                        || (JsonUtils.isNull(language)
                                && (!value.containsKey(Keywords.LANGUAGE) || JsonUtils.isNull(value.get(Keywords.LANGUAGE)))
                                )
                        
                        ){

            // 10.1.
            if ((value.containsKey(Keywords.INDEX)
                    && activePropertyDefinition != null
                    && activePropertyDefinition.hasContainerMapping(Keywords.INDEX))
                || !value.containsKey(Keywords.INDEX)
                    ) {
                
                
                result = value.get(Keywords.VALUE);
            }
        }

        // 11.
        if (JsonUtils.isObject(result)) {
            
            JsonObjectBuilder resultBuilder = Json.createObjectBuilder();
            
            for (Entry<String, JsonValue> entry : result.asJsonObject().entrySet()) {
                resultBuilder.add(
                                activeContext
                                        .compactUri(entry.getKey())
                                        .vocab(true)
                                        .build(), 
                                entry.getValue()
                                );
            }
            
            result = resultBuilder.build();
        }

        // 12.
        return result;
    }
    
}
