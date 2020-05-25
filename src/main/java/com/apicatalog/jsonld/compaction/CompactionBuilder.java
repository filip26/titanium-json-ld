package com.apicatalog.jsonld.compaction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.grammar.ListObject;
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
        JsonObject result = JsonValue.EMPTY_JSON_OBJECT;

        // 11.
        if (elementObject.containsKey(Keywords.TYPE)) {
            
            List<String> compactedTypes = new ArrayList<>();
            
            for (JsonValue type : JsonUtils.toJsonArray(elementObject.get(Keywords.TYPE))) {
                
                compactedTypes.add(activeContext.compacttUri(((JsonString)type).getString()).vocab(true).build());
                
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
        //TODO
        
        // 13.
        return result;
    }
    
}
