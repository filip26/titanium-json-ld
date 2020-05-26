package com.apicatalog.jsonld.compaction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.expansion.MapExpansion;
import com.apicatalog.jsonld.grammar.GraphObject;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#compaction-algorithm">Compaction Algorithm</a>
 *
 */
public final class CompactionBuilder {

    // required
    private ActiveContext activeContext;
    private String activeProperty;
    private JsonValue element;
    
    // optional
    private boolean compactArrays;
    private boolean ordered;
    
    CompactionBuilder(final ActiveContext activeContext, final String activeProperty, final JsonValue element) {
        this.activeContext = activeContext;
        this.activeProperty = activeProperty;
        this.element = element;
        
        // default values
        this.compactArrays = false;
        this.ordered = false;
    }
    
    public static CompactionBuilder with(final ActiveContext activeContext, final String activeProperty, final JsonValue element) {
        return new CompactionBuilder(activeContext, activeProperty, element);
    }
    
    public CompactionBuilder compactArrays(final boolean compactArrays) {
        this.compactArrays = compactArrays;
        return this;
    }
    
    public CompactionBuilder ordered(final boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public JsonValue build() throws JsonLdError {
        
        // 1.
        ActiveContext typeContext = activeContext;
        
        // 2.
        if (JsonUtils.isScalar(element)) {
            return element;
        }
        
        TermDefinition activePropertyDefinition = activeContext.getTerm(activeProperty);
        
        // 3. 
        if (JsonUtils.isArray(element)) {
            
            // 3.1.
            JsonArrayBuilder resultBuilder = Json.createArrayBuilder();
            
            // 3.2.
            for (JsonValue item : element.asJsonArray()) {
                
                // 3.2.1.
                JsonValue compactedItem = CompactionBuilder
                                                .with(typeContext, activeProperty, item)
                                                .compactArrays(compactArrays)
                                                .ordered(ordered)
                                                .build();
                // 3.2.2.                
                if (JsonUtils.isNotNull(compactedItem)) {
                    resultBuilder.add(compactedItem);
                }
            }
            
            JsonArray result = resultBuilder.build();
            
            
            // 3.3.
            if (result.isEmpty() 
                    || result.size() > 1
                    || !compactArrays
                    || Keywords.GRAPH.equals(activeProperty)
                    || Keywords.SET.equals(activeProperty)
                    || (activePropertyDefinition != null
                            && (activePropertyDefinition.hasContainerMapping(Keywords.LIST)
                                || activePropertyDefinition.hasContainerMapping(Keywords.SET))
                            )
                    ) {
                
                return result;
            }
            
            // 3.4.
            return result.get(0);
        }
        
        // 4.
        JsonObject elementObject = element.asJsonObject();
        
        // 5.
        if (activeContext.hasPreviousContext()
                && !elementObject.containsKey(Keywords.VALUE)
                && !(elementObject.containsKey(Keywords.ID)
                        && elementObject.size() == 1)
                ) {
            activeContext = activeContext.getPreviousContext();
        }
        
        // 6.
        if (activePropertyDefinition != null && activePropertyDefinition.hasLocalContext()) {
            activeContext = 
                    activeContext
                            .create(activePropertyDefinition.getLocalContext(), activePropertyDefinition.getBaseUrl())
                            .overrideProtected(true)
                            .build();
                    
        }
        // 7.
        if (elementObject.containsKey(Keywords.VALUE)
                || elementObject.containsKey(Keywords.ID) 
                ) {
            
            JsonValue result = activeContext.compactValue(elementObject, activeProperty).build();
            
            if (JsonUtils.isScalar(result) || activePropertyDefinition != null && Keywords.JSON.equals(activePropertyDefinition.getTypeMapping())) {
                return result;
            }
        }
        
        // 8.
        if (ListObject.isListObject(element)
                && activePropertyDefinition != null
                && activePropertyDefinition.hasContainerMapping(Keywords.LIST)
                ) {
            
            return CompactionBuilder
                        .with(typeContext, activeProperty, elementObject.get(Keywords.LIST))
                        .compactArrays(compactArrays)
                        .ordered(ordered)
                        .build();
        }
        
        // 9.
        boolean insideReverse = Keywords.REVERSE.equals(activeProperty);
        
        // 10.
        Map<String, JsonValue> result = new LinkedHashMap<>();

        // 11.
        if (elementObject.containsKey(Keywords.TYPE)) {
            
            List<String> compactedTypes = new ArrayList<>();
            
            for (JsonValue type : JsonUtils.toJsonArray(elementObject.get(Keywords.TYPE))) {
                
                compactedTypes.add(activeContext.compactUri(((JsonString)type).getString()).vocab(true).build());
                
            }

            Collections.sort(compactedTypes);
            
            for (String term : compactedTypes) {

                // 11.1.
                if (typeContext.containsTerm(term) && typeContext.getTerm(term).hasLocalContext()) {
                    
                    TermDefinition termDefinition = typeContext.getTerm(term);
                    
                    activeContext = 
                            activeContext
                                    .create(termDefinition.getLocalContext(), termDefinition.getBaseUrl())
                                    .propagate(false)
                                    .build();
                }
            }
        }
        
        // 12.
        List<String> expandedProperties = new ArrayList<>(elementObject.keySet());
        
        if (ordered) {
            Collections.sort(expandedProperties);
        }
        
        for (String expandedProperty : expandedProperties) {
            
            JsonValue expandedValue = elementObject.get(expandedProperty);
            
            Map<String, JsonValue> nestResult = new LinkedHashMap<>();
            
            // 12.1.
            if (Keywords.ID.equals(expandedProperty)) {

                JsonValue compactedValue = JsonValue.NULL;
                
                // 12.1.1.
                if (JsonUtils.isString(expandedValue)) {
                    compactedValue = JsonUtils.toJsonValue(activeContext.compactUri(((JsonString)expandedValue).getString()).build());
                }
                
                // 12.1.2.
                String alias = activeContext.compactUri(expandedProperty).vocab(true).build();
                
                // 12.1.3.
                result.put(alias, compactedValue);
                
                continue;
            }
            
            // 12.2.
            if (Keywords.TYPE.equals(expandedProperty)) {

                JsonValue compactedValue = JsonValue.NULL;

                // 12.2.1.
                if (JsonUtils.isString(expandedValue)) {
                    compactedValue = JsonUtils.toJsonValue(typeContext.compactUri(((JsonString)expandedValue).getString()).vocab(true).build());
                    
                // 12.2.2.
                } else if (JsonUtils.isArray(expandedValue)) {

                    // 12.2.2.1.
                    JsonArrayBuilder compactedArray = Json.createArrayBuilder();
                    
                    // 12.2.2.2.
                    for (JsonValue expandedType : expandedValue.asJsonArray()) {

                        // 12.2.2.2.1.
                        String term = typeContext.compactUri(((JsonString)expandedType).getString()).vocab(true).build();

                        // 12.2.2.2.2.
                        compactedArray.add(term);
                    }
                    
                    compactedValue = compactedArray.build();
                                        
                } else {
                    //TODO error
                }

                // 12.2.3.
                String alias = activeContext.compactUri(expandedProperty).vocab(true).build();

                // 12.2.4.
                boolean asArray = (activeContext.inMode(Version.V1_1)
                                        && activeContext.containsTerm(alias)
                                        && activeContext.getTerm(alias).hasContainerMapping(Keywords.SET))
                                    || !compactArrays; 
            
                // 12.2.5.
                MapExpansion.addValue(result, alias, compactedValue, asArray);

                // 12.2.6.
                continue;                
            }
            
            // 12.3.
            if (Keywords.REVERSE.equals(expandedProperty)) {
                //TODO
                
                continue;
            }

            // 12.4.
            if (Keywords.PRESERVE.equals(expandedProperty)) {
                //TODO
            }
            
            // 12.5.
            if (Keywords.INDEX.equals(expandedProperty)
                    && activePropertyDefinition != null
                    && activePropertyDefinition.hasContainerMapping(Keywords.INDEX)
                    ) {
                continue;
                
            // 12.6.
            } else if (Keywords.isOneOf(expandedProperty,
                                            Keywords.DIRECTION,
                                            Keywords.INDEX,
                                            Keywords.LANGUAGE,
                                            Keywords.VALUE
                                            )) {
                // 12.6.1.
                String alias = activeContext.compactUri(expandedProperty).vocab(true).build();
                
                // 12.6.2.
                result.put(alias, expandedValue);
                
                continue;
            }
            
            // 12.7.
            if (JsonUtils.isEmptyArray(expandedValue)) {
                
                // 12.7.1.
                String itemActiveProperty = activeContext
                                                    .compactUri(expandedProperty)
                                                    .value(expandedValue)
                                                    .vocab(true)
                                                    .reverse(insideReverse)
                                                    .build();
                // 12.7.2.
                TermDefinition activeItemTermDefinition = activeContext.getTerm(itemActiveProperty);
                
                if (activeItemTermDefinition != null
                        && activeItemTermDefinition.getNestValue() != null) {

                    if (!Keywords.NEST.equals(activeItemTermDefinition.getNestValue())) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                    

                    
                    //TODO
                    
                // 12.7.3.
                } else {
                    // 12.7.4.
                    MapExpansion.addValue(nestResult, itemActiveProperty, JsonValue.EMPTY_JSON_ARRAY, true);
                }
                

            }
            
            // 12.8.
            for (JsonValue expandedItem : expandedValue.asJsonArray()) {
               
                // 12.8.1.
                String itemActiveProperty = activeContext
                                                .compactUri(expandedProperty)
                                                .value(expandedItem)
                                                .vocab(true)
                                                .reverse(insideReverse)
                                                .build();
                // 12.8.2.
                if (activeContext.containsTerm(itemActiveProperty)
                        && activeContext.getTerm(itemActiveProperty).getNestValue() != null
                        ) {
                    
                    String nestTerm = activeContext.getTerm(itemActiveProperty).getNestValue(); 
                    
                    // 12.8.2.1.
                    if (!Keywords.NEST.equals(nestTerm)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                    //TODO
                    
                    // 12.8.2.2.
                    if (!result.containsKey(nestTerm)) {
                        result.put(nestTerm, JsonValue.EMPTY_JSON_OBJECT);
                    }
                    
                    // 12.8.2.3.
                    nestResult = result.get(nestTerm).asJsonObject();
                    
                // 12.8.3.                    
                } else {
                    nestResult = result;
                }
                
                // 12.8.4.
                Collection<String> container = activePropertyDefinition != null
                                                    ? activePropertyDefinition.getContainerMapping()
                                                    : null;
                
                if (container == null) {
                    container = new LinkedList<>();
                }
                
                // 12.8.5.
                boolean asArray = container.contains(Keywords.SET) 
                                        || Keywords.GRAPH.equals(itemActiveProperty)
                                        || Keywords.LIST.equals(itemActiveProperty)
                                        || !compactArrays;
                
                // 12.8.6.
                JsonValue expandedItemValue = expandedItem;
                
                if (ListObject.isListObject(expandedItem)) {
                    expandedItemValue = expandedItem.asJsonObject().get(Keywords.LIST);
                }

                if (GraphObject.isGraphObject(expandedItem)) {
                    expandedItemValue = expandedItem.asJsonObject().get(Keywords.GRAPH);
                }

                JsonValue compactedItem = CompactionBuilder
                                                .with(typeContext, itemActiveProperty, expandedItemValue)
                                                .compactArrays(compactArrays)
                                                .ordered(ordered)
                                                .build();

                // 12.8.7.
                if (ListObject.isListObject(expandedItem)) {

                    // 12.8.7.1.
                    compactedItem = JsonUtils.toJsonArray(compactedItem);
                    
                    // 12.8.7.2.
                    if (!container.contains(Keywords.LIST)) {

                        // 12.8.7.2.1.
                        String key = activeContext.compactUri(Keywords.LIST).vocab(true).build();
                        
                        compactedItem = Json.createObjectBuilder().add(key, compactedItem).build();
                        
                        // 12.8.7.2.2.
                        if (JsonUtils.isObject(expandedItem) 
                                && expandedItem.asJsonObject().containsKey(Keywords.INDEX)) {

                            String indexKey = activeContext.compactUri(Keywords.INDEX).vocab(true).build();

                            compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject())
                                                .add(indexKey, expandedItem.asJsonObject().get(Keywords.INDEX))
                                                .build();    
                        }
                        
                        // 12.8.7.2.3.
                        MapExpansion.addValue(nestResult, itemActiveProperty, compactedItem, asArray);
                        
                    // 12.8.7.3.
                    } else {
                        nestResult.put(itemActiveProperty, compactedItem);
                    }
                    
                // 12.8.8.
                } else if (GraphObject.isGraphObject(expandedItem)) {
                    
                    //TODO
                // 12.8.9.                    
                } else if (container.contains(Keywords.LANGUAGE)
                            || container.contains(Keywords.INDEX)
                            || container.contains(Keywords.ID)
                            || container.contains(Keywords.TYPE)
                            || !container.contains(Keywords.GRAPH)
                        ) {
                    
                    //TODO
                    
                // 12.8.10.                    
                } else {
                    MapExpansion.addValue(nestResult, itemActiveProperty,compactedItem, asArray);
                }
            }            
        }

        // 13.
        return JsonUtils.toJsonObject(result);
    }
    
}
