package com.apicatalog.jsonld.framing;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
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
                    && JsonLdEmbed.NEVER == embed
                    /*TODO circular */
                    
                    ) {
                addToResult(JsonUtils.toJsonObject(output));
                continue;
            }
            
            // 4.4.
            if (state.isEmbedded() 
                    && JsonLdEmbed.ONCE == embed
                    && state.isProcessed(id)
                    
                    ) {
                addToResult(JsonUtils.toJsonObject(output));
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
                                        .add(Keywords.EMBED, embed.name())
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

                        if (output.containsKey(property)) {
                            //output.put(property, Json.createArrayBuilder(output.get(property).asJsonArray()).add(item).build());
                            
                        } else {
                           
                        }
                        output.put(property, Json.createArrayBuilder().add(item).build());
                    }
                }
                
                // 4.7.4.
                for (String prop : frame.keys().stream().filter(Predicate.not(Keywords::contains)).collect(Collectors.toList())) {
                    
                    if (output.containsKey(prop)) {
                        continue;
                    }
                    
                    JsonValue item = frame.get(prop).asJsonArray().get(0);
                    
                    System.out.println(">> " + prop + " -> " + item);

                    //TODO
                    if (state.isOmitDefault()) {
                        continue;
                    }
                    
                    output.put(prop, Json.createArrayBuilder().add(Keywords.NULL).build());

                    
//                    output.put(prop, Json.createArrayBuilder()
//                                            .add(Json.createObjectBuilder()
//                                                        .add(Keywords.PRESERVE, Json.createArrayBuilder()
//                                                                                .add(Keywords.NULL))).build());
                    
                    System.out.println("TODO " + prop);
                }

                // 4.7.5.
                //TODO
                
                // 4.7.6.
                addToResult(JsonUtils.toJsonObject(output));                
            }

          
        }
    }
        
    private void addToResult(JsonValue output) {
        if (activeProperty == null) {
            parent.put(Integer.toHexString(parent.size()), output);
        } else {
            
            final JsonArrayBuilder array;
            
            if (parent.containsKey(activeProperty)) {
//                array = Json.createArrayBuilder(parent.get(activeProperty).asJsonArray());
                
            } else {
   
            }
            array = Json.createArrayBuilder();
            
            parent.put(activeProperty, array.add(output).build());
        }        
    }
    
}
