package com.apicatalog.jsonld.compaction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.GraphObject;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.Version;

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
                                                .with(activeContext, activeProperty, item)
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
                        .with(activeContext, activeProperty, elementObject.get(Keywords.LIST))
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
                JsonUtils.addValue(result, alias, compactedValue, asArray);

                // 12.2.6.
                continue;                
            }
            
            // 12.3.
            if (Keywords.REVERSE.equals(expandedProperty)) {
                
                // 12.3.1.
                Map<String, JsonValue> compactedMap = new LinkedHashMap<>(CompactionBuilder
                                                .with(activeContext, Keywords.REVERSE, expandedValue)
                                                .compactArrays(compactArrays)
                                                .ordered(ordered)
                                                .build().asJsonObject());
                // 12.3.2.
                for (Entry<String, JsonValue> entry : new HashSet<>(compactedMap.entrySet())) {
                    
                    // 12.3.2.1.
                    if (activeContext.containsTerm(entry.getKey())
                            && activeContext.getTerm(entry.getKey()).isReverseProperty()
                            ) {

                        // 12.3.2.1.1
                        boolean asArray = activeContext.getTerm(entry.getKey()).hasContainerMapping(Keywords.SET)
                                            || !compactArrays;

                        // 12.3.2.1.2.
                        JsonUtils.addValue(result, entry.getKey(), entry.getValue(), asArray);
                        
                        // 12.3.2.1.3.
                        compactedMap.remove(entry.getKey());
                    }
                    
                }
                
                // 12.8.3.
                if (!compactedMap.isEmpty()) {

                    // 12.8.3.1.
                    String alias = activeContext.compactUri(Keywords.REVERSE).vocab(true).build();
                    
                    // 12.8.3.2.                    
                    result.put(alias, JsonUtils.toJsonObject(compactedMap));
                }
                
                // 12.8.4.
                continue;
            }

            // 12.4.
            if (Keywords.PRESERVE.equals(expandedProperty)) {

                // 12.4.1.
                JsonValue compactedValue = CompactionBuilder
                                                .with(activeContext, activeProperty, expandedValue)
                                                .compactArrays(compactArrays)
                                                .ordered(ordered)
                                                .build();
                
                // 12.4.2.
                if (!JsonUtils.isEmptyArray(compactedValue)) {
                    result.put(Keywords.PRESERVE, compactedValue);
                }
                continue;
            }
            
            // 12.5.
            if (Keywords.INDEX.equals(expandedProperty)
                    && activePropertyDefinition != null
                    && activePropertyDefinition.hasContainerMapping(Keywords.INDEX)
                    ) {
                continue;
                
            // 12.6.
            } else if (Keywords.anyMatch(expandedProperty,
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
                if (activeContext.containsTerm(itemActiveProperty)
                        && activeContext.getTerm(itemActiveProperty).getNestValue() != null
                        ) {
                    
                    String nestTerm = activeContext.getTerm(itemActiveProperty).getNestValue(); 
                  
                    // 12.7.2.1.
                    if (!Keywords.NEST.equals(nestTerm) && !Keywords.NEST.equals(activeContext.expandUri(nestTerm).vocab(true).build())) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                    
                    // 12.7.2.2.
                    if (!result.containsKey(nestTerm)) {
                        result.put(nestTerm, JsonValue.EMPTY_JSON_OBJECT);
                    }                    

                    // 12.7.2.3.
                    Map<String, JsonValue> nestResult = new LinkedHashMap<>(result.get(nestTerm).asJsonObject());
                    String nestResultKey = nestTerm;
                    
                    JsonUtils.addValue(nestResult, itemActiveProperty, JsonValue.EMPTY_JSON_ARRAY, true);
                    result.put(nestResultKey, JsonUtils.toJsonObject(nestResult));

                // 12.7.3.                    
                } else {
                    Map<String, JsonValue> nestResult = result;
                    JsonUtils.addValue(nestResult, itemActiveProperty, JsonValue.EMPTY_JSON_ARRAY, true);
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

                Map<String, JsonValue> nestResult = null;
                String nestResultKey = null;

                // 12.8.2.
                if (activeContext.containsTerm(itemActiveProperty)
                        && activeContext.getTerm(itemActiveProperty).getNestValue() != null
                        ) {
                    
                    String nestTerm = activeContext.getTerm(itemActiveProperty).getNestValue(); 
                    
                    // 12.8.2.1.
                    if (!Keywords.NEST.equals(nestTerm) && !Keywords.NEST.equals(activeContext.expandUri(nestTerm).vocab(true).build())) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                    
                    // 12.8.2.2.
                    if (!result.containsKey(nestTerm)) {
                        result.put(nestTerm, JsonValue.EMPTY_JSON_OBJECT);
                    }

                    // 12.8.2.3.
                    nestResult = new LinkedHashMap<>(result.get(nestTerm).asJsonObject());
                    nestResultKey = nestTerm;

                // 12.8.3.                    
                } else {
                    nestResult = result;
                }
                
                // 12.8.4.
                Collection<String> container = activeContext.containsTerm(itemActiveProperty)
                                                ? activeContext.getTerm(itemActiveProperty).getContainerMapping()
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
                    
                } else if (GraphObject.isGraphObject(expandedItem)) {
                    expandedItemValue = expandedItem.asJsonObject().get(Keywords.GRAPH);
                }

                JsonValue compactedItem = CompactionBuilder
                                                .with(activeContext, itemActiveProperty, expandedItemValue)
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
                        JsonUtils.addValue(nestResult, itemActiveProperty, compactedItem, asArray);

                    // 12.8.7.3.
                    } else {
                        nestResult.put(itemActiveProperty, compactedItem);
                    }
                    
                // 12.8.8.
                } else if (GraphObject.isGraphObject(expandedItem)) {
                    
                    boolean followup = false;
                    
                    // 12.8.8.1.
                    if (container.contains(Keywords.GRAPH) && container.contains(Keywords.ID)) {

                        // 12.8.8.1.1.
                        Map<String, JsonValue> mapObject =  nestResult.containsKey(itemActiveProperty) 
                                                    ? new LinkedHashMap<>(nestResult.get(itemActiveProperty).asJsonObject())
                                                    : null;
                        
                        if (mapObject == null) {
                            mapObject = new LinkedHashMap<>();
                        }

                        // 12.8.8.1.2.
                        String mapKey = null;
                        
                        if (expandedItem.asJsonObject().containsKey(Keywords.ID)) {
                            String id = expandedItem.asJsonObject().getString(Keywords.ID);
                            mapKey = activeContext.compactUri(id).build();
                            
                        } else {
                            mapKey = activeContext.compactUri(Keywords.NONE).vocab(true).build();
                        }
   
                        // 12.8.8.1.3.
                        JsonUtils.addValue(mapObject, mapKey, compactedItem, asArray);

                        nestResult.put(itemActiveProperty, JsonUtils.toJsonObject(mapObject));
                        
                    // 12.8.8.2.
                    } else if (container.contains(Keywords.GRAPH) 
                                    && container.contains(Keywords.INDEX)
                                    && GraphObject.isSimpleGraphObject(expandedItem)
                                            ) {
                        // 12.8.8.2.1.
                        Map<String, JsonValue> mapObject = nestResult.containsKey(itemActiveProperty) 
                                                    ? new LinkedHashMap<>(nestResult.get(itemActiveProperty).asJsonObject())
                                                    : null;
        
                        if (mapObject == null) {
                            mapObject = new LinkedHashMap<>();
                        }
                        
                        // 12.8.8.2.2.
                        String mapKey  = expandedItem.asJsonObject().containsKey(Keywords.INDEX)
                                            ? expandedItem.asJsonObject().getString(Keywords.INDEX)
                                            : null;
                                            
                        if (mapKey == null) {
                            mapKey = Keywords.NONE;
                        }
   
                        // 12.8.8.2.3.
                        JsonUtils.addValue(mapObject, mapKey, compactedItem, asArray);

                        nestResult.put(itemActiveProperty, JsonUtils.toJsonObject(mapObject));
                        
                    // 12.8.8.3.                        
                    } else if (container.contains(Keywords.GRAPH) 
                            && GraphObject.isSimpleGraphObject(expandedItem)
                                    ) {

                        // 12.8.8.3.1.
                        if (JsonUtils.isArray(compactedItem) && compactedItem.asJsonArray().size() > 1) {
                            compactedItem = Json.createObjectBuilder().add(
                                                        activeContext
                                                            .compactUri(Keywords.INCLUDED)
                                                            .vocab(true)
                                                            .build(),
                                                        compactedItem
                                                    ).build();
                        }

                        // 12.8.8.3.2.
                        JsonUtils.addValue(nestResult, itemActiveProperty, compactedItem, asArray);
                        
                        
                    } else {
                        followup = true;
                    }

                    // 12.8.8.4.
                    if (!container.contains(Keywords.GRAPH) || followup) {
                        // 12.8.8.4.1.
                        compactedItem = Json.createObjectBuilder().add(
                                activeContext
                                    .compactUri(Keywords.GRAPH)
                                    .vocab(true)
                                    .build(),
                                compactedItem
                            ).build(); 
                        
                        // 12.8.8.4.2.
                        if (expandedItem.asJsonObject().containsKey(Keywords.ID)) {

                            compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).add(
                                    activeContext
                                        .compactUri(Keywords.ID)
                                        .vocab(true)
                                        .build(),
                                    activeContext
                                        .compactUri(expandedItem.asJsonObject().getString(Keywords.ID))
                                        .build()
                                ).build(); 
                        }
                                                        
                        // 12.8.8.4.3.
                        if (expandedItem.asJsonObject().containsKey(Keywords.INDEX)) {
                            
                            compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).add(
                                    activeContext
                                        .compactUri(Keywords.INDEX)
                                        .vocab(true)
                                        .build(),
                                    expandedItem.asJsonObject().getString(Keywords.INDEX)
                                ).build(); 
                        }

                        // 12.8.8.4.4.
                        JsonUtils.addValue(nestResult, itemActiveProperty, compactedItem, asArray);
                    }
                    
                // 12.8.9.                    
                } else if ((container.contains(Keywords.LANGUAGE)
                            || container.contains(Keywords.INDEX)
                            || container.contains(Keywords.ID)
                            || container.contains(Keywords.TYPE))
                            && !container.contains(Keywords.GRAPH)
                        ) {

                    // 12.8.9.1
                    Map<String, JsonValue> mapObject = nestResult.containsKey(itemActiveProperty)
                                                            ? new LinkedHashMap<>(nestResult.get(itemActiveProperty).asJsonObject())
                                                            : new LinkedHashMap<>()
                                                            ;
                    // 12.8.9.2.
                    String keyToCompact = null;
                    
                    if (container.contains(Keywords.LANGUAGE)) {
                        keyToCompact = Keywords.LANGUAGE;
                        
                    } else if (container.contains(Keywords.INDEX)) {
                        keyToCompact = Keywords.INDEX;
                        
                    } else if (container.contains(Keywords.ID)) {
                        keyToCompact = Keywords.ID;
                        
                    } else if (container.contains(Keywords.TYPE)) {
                        keyToCompact = Keywords.TYPE;
                    }

                    String containerKey = activeContext.compactUri(keyToCompact).vocab(true).build();

                    // 12.8.9.3.
                    String indexKey = null;
                    if (activeContext.containsTerm(itemActiveProperty)) {
                        indexKey = activeContext.getTerm(itemActiveProperty).getIndexMapping();
                    }
                    
                    if (indexKey == null) {
                        indexKey = Keywords.INDEX;
                    }
                    
                    String mapKey = null;
                    
                    // 12.8.9.4.
                    if (container.contains(Keywords.LANGUAGE)
                            && expandedItem.asJsonObject().containsKey(Keywords.VALUE)
                            ) {

                        if (JsonUtils.isObject(compactedItem)) {
                            compactedItem = compactedItem.asJsonObject().get(Keywords.VALUE);
                        }
                        
                        if (expandedItem.asJsonObject().containsKey(Keywords.LANGUAGE)) {
                            
                            mapKey = expandedItem.asJsonObject().getString(Keywords.LANGUAGE);
                        }

                    // 12.8.9.5.                        
                    } else if (container.contains(Keywords.INDEX)
                                && Keywords.INDEX.equals(indexKey)) {

                        if (expandedItem.asJsonObject().containsKey(Keywords.INDEX)) {
                            
                            mapKey = expandedItem.asJsonObject().getString(Keywords.INDEX);
                        }
                        
                    // 12.8.9.6.                        
                    } else if (container.contains(Keywords.INDEX)
                                && !Keywords.INDEX.equals(indexKey)) {

                        // 12.8.9.6.1.
                        containerKey = activeContext
                                                .compactUri(indexKey)
                                                .vocab(true)
                                                .build();
                        
                        // 12.8.9.6.2.
                        if (JsonUtils.isObject(compactedItem) && compactedItem.asJsonObject().containsKey(containerKey)) {

                            JsonValue containerValue = compactedItem.asJsonObject().get(containerKey);
                            
                            if (JsonUtils.isString(containerValue)) {
                                mapKey = ((JsonString)containerValue).getString();
                                
                                // 12.8.9.6.3.
                                compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();

                            } else if (JsonUtils.isArray(containerValue) && !JsonUtils.isEmptyArray(containerValue)) {
                                
                                mapKey = containerValue.asJsonArray().getString(0);

                                // 12.8.9.6.3.
                                if (containerValue.asJsonArray().size() > 1) {

                                    JsonValue containerKeyValue = null;

                                    if (containerValue.asJsonArray().size() == 2) {
                                        containerKeyValue = containerValue.asJsonArray().get(1);
                                        
                                    } else {
                                        containerKeyValue = Json.createArrayBuilder(containerValue.asJsonArray()).remove(0).build();
                                    }

                                    compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject())
                                                        .remove(containerKey)
                                                        .add(containerKey, containerKeyValue)
                                                        .build();

                                } else {
                                    compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                                }
                            }
                        }

                    // 12.8.9.7.                        
                    } else if (container.contains(Keywords.ID)) {
           
                        if (JsonUtils.isObject(compactedItem)
                                && compactedItem.asJsonObject().containsKey(containerKey)) {

                            mapKey = compactedItem.asJsonObject().getString(containerKey);
                            
                            Map<String, JsonValue> compactedItemMap = new LinkedHashMap<>(compactedItem.asJsonObject());
                            compactedItemMap.remove(containerKey);
                            
                            compactedItem = JsonUtils.toJsonObject(compactedItemMap);
                            
                        } else {
                           // mapKey = null;  //TODO needs to be revised, removal candidate
                        }

                    // 12.8.9.8.
                    } else if (container.contains(Keywords.TYPE)) {

                        // 12.8.9.8.1.
                        if (JsonUtils.isObject(compactedItem) 
                                && compactedItem.asJsonObject().containsKey(containerKey)) {
                            
                            JsonValue compactedKeyValue = compactedItem.asJsonObject().get(containerKey);
                            
                            if (JsonUtils.isNotNull(compactedKeyValue)) {
                                
                                JsonArray compactedKeyArray = JsonUtils.toJsonArray(compactedKeyValue);
                                
                                mapKey = compactedKeyArray.getString(0);

                                if (compactedKeyArray.size() > 1) {

                                    JsonValue compactedKeyArrayValue = null;
                                    if (compactedKeyArray.size() == 2) {

                                        compactedKeyArrayValue = compactedKeyArray.get(1);
                                        
                                    } else {
                                        compactedKeyArrayValue = Json.createArrayBuilder(compactedKeyArray).remove(0).build();   
                                    }
                                                                         
                                    compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject())
                                                        .remove(containerKey)
                                                        .add(containerKey, compactedKeyArrayValue)
                                                        .build();

                                } else {
                                    compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                                }
                                
                                
                            } else {
                                compactedItem = Json.createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                            }
                        }
                                                
                        // 12.8.9.8.4.
                        if (JsonUtils.isObject(compactedItem) && compactedItem.asJsonObject().size() == 1) {
                            
                            String epandedKey = activeContext.expandUri(compactedItem.asJsonObject().keySet().iterator().next()).vocab(true).build();
                            
                            if (Keywords.ID.equals(epandedKey)) {

                                JsonObject map = Json.createObjectBuilder().add(Keywords.ID, expandedItem.asJsonObject().get(Keywords.ID)).build();
                                
                                compactedItem = CompactionBuilder
                                                    .with(activeContext, itemActiveProperty, map)
                                                    .build();
                            }   
                        }
                    }
                    
                    // 12.8.9.9.
                    if (mapKey == null) {
                        mapKey = activeContext.compactUri(Keywords.NONE).vocab(true).build();
                    } 
                    // 12.8.9.10.
                    JsonUtils.addValue(mapObject, mapKey, compactedItem, asArray);


                    nestResult.put(itemActiveProperty, JsonUtils.toJsonObject(mapObject));

                // 12.8.10.                    
                } else {
                    JsonUtils.addValue(nestResult, itemActiveProperty, compactedItem, asArray);
                    
                }

                if (nestResult != null && nestResultKey != null) {
                    result.put(nestResultKey, JsonUtils.toJsonObject(nestResult));
                }
            }
        }

        // 13.
        return JsonUtils.toJsonObject(result);
    }
    
}
