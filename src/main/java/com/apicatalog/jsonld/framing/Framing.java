package com.apicatalog.jsonld.framing;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdEmbed;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-framing/#framing-algorithm">Framing Algorithm</a>
 *
 */
public final class Framing {

    // required
    private final FramingState state;
    private final List<String> subjects;
    private final Frame frame;
    private final Map<String, JsonValue> parent;
    
    private String activeProperty;
    
    // optional
    private boolean ordered;
    
    private Framing(FramingState state, List<String> subjects, Frame frame, Map<String, JsonValue> parent, String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
    }
    
    public static final Framing with(FramingState state, List<String> subjects, Frame frame, Map<String, JsonValue> parent, String activeProperty) {
        return new Framing(state, subjects, frame, parent, activeProperty);
    }
    
    public Framing ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public void frame() throws JsonLdError {

        // 2.
        final JsonLdEmbed embed = frame.getEmbed(state.getEmbed());
                
        final boolean explicit = frame.getExplicit(state.isExplicitInclusion());

        final boolean requireAll = frame.getRequireAll(state.isRequireAll());

        // 3.
        final List<String> matchedSubjects = 
                                FrameMatcher
                                    .with(state, frame, requireAll)
                                    .match(subjects);
                
        // 4.
        if (ordered) {
            Collections.sort(matchedSubjects);
        }
          
        for (final String id : matchedSubjects) {
            
            final Map<String, JsonValue> node = state.getGraphMap().get(state.getGraphName(), id);
            
            final String nodeId = JsonUtils.isString(node.get(Keywords.ID))
                                ? ((JsonString)node.get(Keywords.ID)).getString()
                                : null;

            // 4.1.
            Map<String, JsonValue> output = new LinkedHashMap<>();
            output.put(Keywords.ID, Json.createValue(id));
            
            
            if (activeProperty == null) {
                state.clearDone();
            }
            
            // 4.2.
            if (!state.isEmbedded() && state.isDone(id))  {
                continue;
            }

            // 4.3.
            if (state.isEmbedded() 
                    && (JsonLdEmbed.NEVER == embed
                            || state.isParent(nodeId)
                            ) 
                    
                    ) {
                addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
                continue;
            }
            
            // 4.4.
            if (state.isEmbedded() 
                    && JsonLdEmbed.ONCE == embed
                    && state.isDone(id)
                    
                    ) {
                addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
                continue;
            }

            state.markDone(id);
            state.addParent(nodeId);

            // 4.5.
            if (state.getGraphMap().contains(id)) {
      
                // 4.5.1.
                boolean recurse;
                Frame subframe;
                
                if (!frame.contains(Keywords.GRAPH)) {
                    recurse = !Keywords.MERGED.equals(state.getGraphName());
                    subframe = Frame.of(JsonValue.EMPTY_JSON_OBJECT);
                    
                // 4.5.2.
                } else {
                    recurse = !Keywords.ID.equals(id) && !Keywords.DEFAULT.equals(id);
                    
                    if (JsonUtils.isObject(frame.get(Keywords.GRAPH))
                            || JsonUtils.isArray(frame.get(Keywords.GRAPH))
                            ) {
                        
                        subframe = Frame.of((JsonStructure)frame.get(Keywords.GRAPH));
                        
                    } else {
                        subframe = Frame.of(JsonValue.EMPTY_JSON_OBJECT);
                    }
                }
                
                // 4.5.3.
                if (recurse) {
                    
                    final FramingState graphState = new FramingState(state);
                    
                    graphState.setGraphName(id);
                    graphState.setEmbedded(false);
                    
                    Framing.with(
                                graphState, 
                                List.copyOf(state.getGraphMap().get(id).keySet()), 
                                subframe, 
                                output, 
                                Keywords.GRAPH
                                )
                            .ordered(ordered)
                            .frame();
                }
            }
            
            // 4.6.
            if (frame.contains(Keywords.INCLUDED)) {
                
                FramingState includedState = new FramingState(state);
                includedState.setEmbedded(false);
                
                Framing.with(
                            includedState, 
                            subjects, 
                            Frame.of((JsonStructure)frame.get(Keywords.INCLUDED)),
                            output, 
                            Keywords.INCLUDED
                            )
                        .ordered(ordered)
                        .frame();                
            }

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

                            JsonValue listFrame = null;
                            
                            if (frame.contains(property)
                                    && JsonUtils.isNotEmptyArray(frame.get(property))
                                    && JsonUtils.isObject(frame.get(property).asJsonArray().get(0))
                                ) {
                                listFrame = frame.get(property).asJsonArray().get(0).asJsonObject().get(Keywords.LIST);
                            }

                            if (listFrame == null) {
                                listFrame = Json.createObjectBuilder()
                                        .add(Keywords.EMBED, "@".concat(embed.name().toLowerCase()))
                                        .add(Keywords.EXPLICIT, explicit)
                                        .add(Keywords.REQUIRE_ALL, requireAll)
                                        .build();
                            }

                            final JsonArrayBuilder list = Json.createArrayBuilder();
                            
                            for (final JsonValue listItem : JsonUtils.toJsonArray(item.asJsonObject().get(Keywords.LIST))) {

                                // 4.7.3.1.1.
                                if (NodeObject.isNodeReference(listItem)) {

                                    FramingState listState = new FramingState(state);
                                    listState.setEmbedded(true);
                                    

                                    Map<String, JsonValue> listResult = new LinkedHashMap<>();
                                    
                                    Framing.with(
                                                listState, 
                                                Arrays.asList(listItem.asJsonObject().getString(Keywords.ID)),
                                                Frame.of((JsonStructure)listFrame), 
                                                listResult, 
                                                Keywords.LIST)
                                            .ordered(ordered)
                                            .frame();

                                    if (listResult.containsKey(Keywords.LIST)) {
                                        list.add(listResult.get(Keywords.LIST));
                                    }
                                    
                                // 4.7.3.1.2.
                                } else {
                                    list.add(listItem);
                                }
                            }                           
                            output.put(property, Json.createArrayBuilder().add(Json.createObjectBuilder().add(Keywords.LIST, list)).build());

                        
                    } else if (NodeObject.isNodeReference(item)) {

                        FramingState clonedState = new FramingState(state);
                        clonedState.setEmbedded(true);

                        Framing.with(
                                    clonedState, 
                                    Arrays.asList(item.asJsonObject().getString(Keywords.ID)),
                                    Frame.of((JsonStructure)subframe), 
                                    output, 
                                    property)
                                .ordered(ordered)                        
                                .frame();
                        
                    } else if (ValueObject.isValueObject(item)) {                        
                        if ((Frame.of((JsonStructure)subframe)).matchValue(item)) {
                            JsonUtils.addValue(output, property, item, true);
                        }
                        
                    } else {
                        

                        JsonUtils.addValue(output, property, item, true);
                    }
                }                               
            }

            // 4.7.4. - default values
            for (String property : frame.keys()) {
                if (output.containsKey(property)
                        || !Keywords.TYPE.equals(property) && Keywords.matchForm(property)
                        || Keywords.TYPE.equals(property) && !frame.isDefaultOjbect(property)
                        ) {
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

                output.put(property, Json.createArrayBuilder()
                                        .add(Json.createObjectBuilder()
                                                    .add(Keywords.PRESERVE, 
                                                            Json.createArrayBuilder().add(
                                                            defaultValue))).build());
                
            }

            // 4.7.5. - reverse properties
            if (frame.contains(Keywords.REVERSE)) {
                
                final JsonValue reverseObject = frame.get(Keywords.REVERSE);
                
                if (JsonUtils.isObject(reverseObject)) {
                
                    for (final String reverseProperty :  reverseObject.asJsonObject().keySet()) {
                      
                        final JsonValue subframe = reverseObject.asJsonObject().get(reverseProperty);
                        
                        for (final String subjectProperty : state.getGraphMap().get(state.getGraphName()).keySet()) {
                         
                            final JsonValue nodeValues = state.getGraphMap().get(state.getGraphName(), subjectProperty, reverseProperty);

                            if (nodeValues != null
                                    && JsonUtils.toJsonArray(nodeValues)
                                                .stream()
                                                .filter(JsonUtils::isObject)
                                                .map(JsonObject.class::cast)
                                                .filter(v -> v.containsKey(Keywords.ID))
                                                .map(v -> v.getString(Keywords.ID))
                                                .anyMatch(vid -> Objects.equals(vid, id))
                                    ) {

                                final Map<String, JsonValue> reverseMap;
                                
                                final Map<String, JsonValue> revereseResult = new LinkedHashMap<>();
                                
                                FramingState reverseState = new FramingState(state);
                                reverseState.setEmbedded(true);
                                
                                Framing.with(
                                            reverseState, 
                                            Arrays.asList(subjectProperty), 
                                            Frame.of((JsonStructure)subframe),
                                            revereseResult, 
                                            null)
                                        .ordered(ordered)
                                        .frame();

                                if (output.containsKey(Keywords.REVERSE)) {                        
                                    reverseMap = new LinkedHashMap<>(output.get(Keywords.REVERSE).asJsonObject());
                                    
                                } else {
                                    reverseMap = new LinkedHashMap<>();
                                }

                                JsonUtils.addValue(reverseMap, reverseProperty, JsonUtils.toJsonArray(revereseResult.values()), true);
                                output.put(Keywords.REVERSE, JsonUtils.toJsonObject(reverseMap));
                            }
                        }                        
                    }
                }
            }
            
            state.removeLastParent();

            // 4.8.
            addToResult(parent, activeProperty, JsonUtils.toJsonObject(output));
        }
    }
        
    private static void addToResult(Map<String, JsonValue> result, String property, JsonValue value) {
        if (property == null) {
            result.put(Integer.toHexString(result.size()), value);
            
        } else {
            JsonUtils.addValue(result, property, value, true);
        }        
    }
}
