package com.apicatalog.jsonld.compaction;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-compaction">Value Compaction</a>
 *
 */
public final class ValueCompaction {

    // mandatory
    private final ActiveContext activeContext;
    
    private ValueCompaction(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }
    
    public static ValueCompaction with(ActiveContext activeContext) {
        return new ValueCompaction(activeContext);
    }
    
    public JsonValue compact(final JsonObject value, final String activeProperty) throws JsonLdError {

        // 1.
        JsonValue result = value;
        
        // 2.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }
        
        final Optional<TermDefinition> activePropertyDefinition = activeContext.getTerm(activeProperty);
        
        // 4. - 5.
        JsonValue language = null; 
        DirectionType direction = null;

        if (activePropertyDefinition.isPresent()) {
            language = activePropertyDefinition.get().getLanguageMapping(); 
            direction = activePropertyDefinition.get().getDirectionMapping();
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
            if (activePropertyDefinition.isPresent() 
                            && Keywords.ID.equals(activePropertyDefinition.get().getTypeMapping())) {
                
                result = JsonUtils.toJsonValue(activeContext
                                                .uriCompaction()
                                                .compact(value.getString(Keywords.ID)));

            // 6.2.
            } else if (activePropertyDefinition.isPresent()
                            && Keywords.VOCAB.equals(activePropertyDefinition.get().getTypeMapping())) {
                
                result = JsonUtils.toJsonValue(activeContext
                                                .uriCompaction()
                                                .vocab(true)
                                                .compact(value.getString(Keywords.ID)));
            }
        // 7.
        } else if (value.containsKey(Keywords.TYPE)
                    && activePropertyDefinition.isPresent()
                    && JsonUtils.contains(
                                    activePropertyDefinition.get().getTypeMapping(),
                                    value.get(Keywords.TYPE)
                                        )
                    ) {

            result = value.get(Keywords.VALUE);

        // 8.
        } else if (activePropertyDefinition.isPresent() 
                        && Keywords.NONE.equals(activePropertyDefinition.get().getTypeMapping())
                        || (value.containsKey(Keywords.TYPE)
                                && (activePropertyDefinition.isEmpty()
                                        || !JsonUtils.contains(
                                                activePropertyDefinition.get().getTypeMapping(),
                                                value.get(Keywords.TYPE)
                                                )
                                        ))
                ) {

            // 8.1.
            JsonArrayBuilder types = Json.createArrayBuilder();
            
            JsonValue resultTypes = result.asJsonObject().get(Keywords.TYPE);
            
            if (JsonUtils.isNotNull(resultTypes)) {
                for (JsonValue type : JsonUtils.toJsonArray(resultTypes)) {
    
                    types.add(activeContext.uriCompaction().vocab(true).compact(((JsonString)type).getString()));                    
                }
                
                Map<String, JsonValue> resultMap = new LinkedHashMap<>(result.asJsonObject());
                resultMap.put(Keywords.TYPE, types.build());
                
                result = JsonUtils.toJsonObject(resultMap);
            }
            
        // 9.
        } else if (JsonUtils.isNotString(value.get(Keywords.VALUE))) {
            
            if (!value.containsKey(Keywords.INDEX) 
                    || activePropertyDefinition.map(td -> td.hasContainerMapping(Keywords.INDEX)).orElse(false)
                    ) {
                result = value.get(Keywords.VALUE);
            }

        // 10.
        } else if ((((value.containsKey(Keywords.LANGUAGE)
                                && JsonUtils.isString(value.get(Keywords.LANGUAGE))
                                && JsonUtils.isString(language)
                                && (((JsonString)language).getString().equalsIgnoreCase(value.getString(Keywords.LANGUAGE)))
                                )
                        || (JsonUtils.isNull(language)
                                && (!value.containsKey(Keywords.LANGUAGE) || JsonUtils.isNull(value.get(Keywords.LANGUAGE))))
                        )
                        && ((direction != null && direction != DirectionType.NULL
                                && value.containsKey(Keywords.DIRECTION)
                                && JsonUtils.isString(value.get(Keywords.DIRECTION))
                                && direction == DirectionType.valueOf(value.getString(Keywords.DIRECTION).toUpperCase())
                                )
                                || ((direction == null || direction == DirectionType.NULL)
                                    && (!value.containsKey(Keywords.DIRECTION)
                                    || DirectionType.NULL == DirectionType.valueOf(value.getString(Keywords.DIRECTION).toUpperCase())
                                ))
                                )
                        )
                    && ((value.containsKey(Keywords.INDEX)
                            && activePropertyDefinition.isPresent()
                            && activePropertyDefinition.get().hasContainerMapping(Keywords.INDEX))
                        || !value.containsKey(Keywords.INDEX)
                            )
                ){

                result = value.get(Keywords.VALUE);
        }

        // 11.
        if (JsonUtils.isObject(result)) {

            JsonObjectBuilder resultBuilder = Json.createObjectBuilder();
            
            for (Entry<String, JsonValue> entry : result.asJsonObject().entrySet()) {
                resultBuilder.add(
                                activeContext
                                        .uriCompaction()
                                        .vocab(true)
                                        .compact(entry.getKey()), 
                                entry.getValue()
                                );
            }
            
            result = resultBuilder.build();
        }

        // 12.
        return result;
    }
    
}
