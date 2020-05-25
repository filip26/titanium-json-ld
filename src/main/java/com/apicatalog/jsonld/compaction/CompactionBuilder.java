package com.apicatalog.jsonld.compaction;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.grammar.Keywords;
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
        
        //TODO
        return JsonObject.EMPTY_JSON_OBJECT;
    }
    
}
