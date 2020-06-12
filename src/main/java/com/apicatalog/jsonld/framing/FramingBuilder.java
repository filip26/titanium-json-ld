package com.apicatalog.jsonld.framing;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdEmbed;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-framing/#framing-algorithm">Framing Algorithm</a>
 *
 */
public final class FramingBuilder {

    // required
    private FramingState state;
    private List<String> subjects;
    private Frame frame;
    private Map<String, JsonValue> parent;
    
    private String activeProperty;
    
    // optional
    private boolean ordered;
    
    private FramingBuilder(FramingState state, List<String> subjects, Frame frame, Map<String, JsonValue> parent, String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
    }
    
    public static final FramingBuilder with(FramingState state, List<String> subjects, Frame frame, Map<String, JsonValue> parent, String activeProperty) {
        return new FramingBuilder(state, subjects, frame, parent, activeProperty);
    }
    
    public void build() throws JsonLdError {

        // 2.
        JsonLdEmbed embed = frame.getEmbed(state.getEmbed());
                
        boolean explicit = frame.getExplicit(state.isExplicitInclusion());

        boolean requireAll = frame.getRequireAll(state.isRequireAll());

        // 3.
        final List<String> matchedSubjects = 
                                FrameMatcher
                                    .with(state, subjects, frame, requireAll)
                                    .match();
                
        // 4.
        if (ordered) {
            Collections.sort(matchedSubjects);
        }

        for (final String id : matchedSubjects) {
            
            final Map<String, JsonValue> node = state.getGraphMap().get(state.getGraphName(), id);
            
            // 4.1.
            Map<String, JsonValue> output = new LinkedHashMap<>();
            output.put(Keywords.ID, Json.createValue(id));
            
            
            // 4.2.
            if (!state.isEmbedded() && state.isProcessed(id))  {
                continue;
            }

            // 4.3.
            if (state.isEmbedded() 
                    && (JsonLdEmbed.NEVER == embed
                            || state.isProcessed(id)
                            ) /*TODO circular */
                    
                    ) {
                addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
                continue;
            }
            
            // 4.4.
            if (state.isEmbedded() 
                    && JsonLdEmbed.ONCE == embed
                    && state.isProcessed(id)
                    
                    ) {
                addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
                continue;
            }

            state.markProcessed(id);

            // 4.5.
            //TODO
            
            // 4.6.
            //TODO
            
            // 4.7.    
            for (final String property : state.getGraphMap().properties(state.getGraphName(), id, ordered)) {
                final JsonValue objects = state.getGraphMap().get(state.getGraphName(), id, property);
                
                // 4.7.1.
                if (Keywords.contains(property)) {
                    output.put(property, objects);
                    continue;
                } 
                
                // 4.7.2.
                if (explicit && !frame.contains(property)) {
                    continue;
                }

                // 4.7.3.
                for (final JsonValue item : JsonUtils.toJsonArray(objects)) {

                    JsonValue subframe = frame.get(property);

                    if (subframe == null) {
                        subframe = Json.createObjectBuilder()
                                        .add(Keywords.EMBED, "@".concat(embed.name().toLowerCase()))
                                        .add(Keywords.EXPLICIT, explicit)
                                        .add(Keywords.REQUIRE_ALL, requireAll)
                                        .build();
                    }

                    // 4.7.3.1.
                    if (ListObject.isListObject(item)) {
                        System.out.println("TODO: LIST");
                        
                    } else if (NodeObject.isNodeReference(item)) {
                        
                        FramingState clonedState = new FramingState(state);
                        clonedState.setEmbedded(true);

                        FramingBuilder.with(
                                    clonedState, 
                                    Arrays.asList(item.asJsonObject().getString(Keywords.ID)),
                                    Frame.of((JsonStructure)subframe), 
                                    output, 
                                    property)
                            .build();
                        
                    } else {

                        JsonUtils.addValue(output, property, item, true);
                    }
                }                               
            }
            
            
            // 4.7.4.
            for (String property : frame.keys().stream().filter(Predicate.not(Keywords::contains)).collect(Collectors.toList())) {

                if (output.containsKey(property)) {
                    continue;
                }
                
                // 4.7.4.2.
                final JsonObject propertyFrame;
                
                if (JsonUtils.isArray(frame.get(property)) && JsonUtils.isNotEmptyArray(frame.get(property))) {
                    propertyFrame = frame.get(property).asJsonArray().getJsonObject(0);
                } else {
                    propertyFrame = JsonValue.EMPTY_JSON_OBJECT;
                }
                
                // 4.7.4.3.
                if (Frame.getBoolean(propertyFrame, Keywords.OMIT_DEFAULT, state.isOmitDefault())) {
                    continue;
                }
                                
                // 4.7.4.4.
                JsonValue defaultValue = propertyFrame.get(Keywords.DEFAULT);
                
                if (JsonUtils.isNull(defaultValue)) {
                    defaultValue = Json.createValue(Keywords.NULL);
                }

//                output.put(property, Json.createArrayBuilder()
//                        .add(defaultValue).build());
//

                output.put(property, Json.createArrayBuilder()
                                        .add(Json.createObjectBuilder()
                                                    .add(Keywords.PRESERVE, 
                                                            Json.createArrayBuilder().add(
                                                            defaultValue))).build());
                
            }

            // 4.7.5.
            //TODO
            
            state.release(id);

            // 4.8.
            addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
          
        }
    }
        
    private static void addToResult(Map<String, JsonValue> result, String property, JsonValue value) {
        
        if (property == null) {
            result.put(Integer.toHexString(result.size()), value);
        } else {

            JsonUtils.addValue(result, property, value, true);
            
//            final JsonArrayBuilder array;
//            
//            if (parent.containsKey(activeProperty)) {
//                array = Json.createArrayBuilder(parent.get(activeProperty).asJsonArray());
//                
//            } else {
//                array = Json.createArrayBuilder();   
//            }
//
//            
//            parent.put(activeProperty, array.add(output).build());
        }        
    }
    
}
