/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.framing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdEmbed;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.lang.JsonLdNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-framing/#framing-algorithm">Framing
 *      Algorithm</a>
 *
 */
public final class Framing {

    // required
    private final FramingState state;
    private final List<String> subjects;
    private final Frame frame;
    private final Map<String, Object> parent;

    private String activeProperty;

    // optional
    private boolean ordered;

    private Framing(FramingState state, List<String> subjects, Frame frame, Map<String, Object> parent, String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
    }

    public static final Framing with(FramingState state, List<String> subjects, Frame frame,
            Map<String, Object> parent, String activeProperty) {
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
        final List<String> matchedSubjects = FrameMatcher
                .with(state, frame, requireAll)
                .match(subjects);

        // 4.
        for (final var id : Utils.index(matchedSubjects, ordered)) {

            final Map<String, ?> node = (Map) state.getGraphMap().find(state.getGraphName(), id).orElse(Collections.emptyMap());

            final String nodeId = node.get(Keywords.ID) instanceof String stringId
                    ? stringId
                    : null;

            // 4.1.
            final var output = new LinkedHashMap<String, Object>();
            output.put(Keywords.ID, id);

            if (activeProperty == null) {
                state.clearDone();
            }

            // 4.2.
            if (!state.isEmbedded() && state.isDone(id)) {
                continue;
            }

            // 4.3.
            if (state.isEmbedded()
                    && (JsonLdEmbed.NEVER == embed
                            || state.isParent(nodeId))) {
                addToResult(parent, activeProperty, output);
                continue;
            }

            // 4.4.
            if (state.isEmbedded()
                    && JsonLdEmbed.ONCE == embed
                    && state.isDone(id)

            ) {
                addToResult(parent, activeProperty, output);
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
                    subframe = Frame.EMPTY;

                } else {
                    // 4.5.2.
                    recurse = !Keywords.MERGED.equals(id) && !Keywords.DEFAULT.equals(id);

                    final var graph = frame.get(Keywords.GRAPH);

                    if (graph instanceof Map || graph instanceof Collection) {
                        subframe = Frame.of(graph);

                    } else {
                        subframe = Frame.EMPTY;
                    }
                }

                // 4.5.3.
                if (recurse) {

                    final FramingState graphState = new FramingState(state);

                    graphState.setGraphName(id);
                    graphState.setEmbedded(false);

                    Framing.with(
                            graphState,
                            new ArrayList<>(state.getGraphMap().find(id).map(Map::keySet).orElse(Collections.emptySet())),
                            subframe,
                            output,
                            Keywords.GRAPH)
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
                        Frame.of(frame.get(Keywords.INCLUDED)),
                        output,
                        Keywords.INCLUDED)
                        .ordered(ordered)
                        .frame();
            }

            // 4.7.
            for (final String property : Utils.index(state.getGraphMap().properties(state.getGraphName(), id), ordered)) {
                final var objects = state.getGraphMap().get(state.getGraphName(), id, property);

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
                for (final var item : NativeAdapter.asCollection(objects)) {

                    var subframe = frame.get(property);

                    if (subframe == null) {
                        subframe = Map.of(Keywords.EMBED, "@".concat(embed.name().toLowerCase()),
                                Keywords.EXPLICIT, explicit,
                                Keywords.REQUIRE_ALL, requireAll);
                    }

                    // 4.7.3.1.
                    if (JsonLdNode.isList(item)) {

                        Object listFrameValue = null;

                        var propertyValue = frame.get(property);

                        if (propertyValue instanceof Collection array
                                && !array.isEmpty()
                                && array.iterator().next() instanceof Map map) {
//                                && !JsonUtils.isEmptyArray()
//                                && JsonUtils.isObject(frame.get(property).asJsonArray().get(0))) {
                            listFrameValue = map.get(Keywords.LIST);
                        }

                        if (listFrameValue == null) {
                            listFrameValue = Map.of(
                                    Keywords.EMBED, "@".concat(embed.name().toLowerCase()),
                                    Keywords.EXPLICIT, explicit,
                                    Keywords.REQUIRE_ALL, requireAll);
                        }

                        final Frame listFrame = Frame.of(listFrameValue);

                        final var list = new ArrayList<Object>();

                        for (final var listItem : NativeAdapter.asCollection(
                                ((Map) item).get(Keywords.LIST))) {

                            // 4.7.3.1.1.
                            if (JsonLdNode.isReference(listItem)) {

                                FramingState listState = new FramingState(state);
                                listState.setEmbedded(true);

                                final var listResult = new LinkedHashMap<String, Object>();

                                Framing.with(
                                        listState,
                                        Arrays.asList(((Map<String, String>) listItem).get(Keywords.ID)),
//                                        Arrays.asList(listItem.asJsonObject().getString(Keywords.ID)),
                                        listFrame,
                                        listResult,
                                        Keywords.LIST)
                                        .ordered(ordered)
                                        .frame();

                                var existingList = listResult.get(Keywords.LIST);

                                if (existingList != null) {
                                    list.add(existingList);
                                }

                                // 4.7.3.1.2.
                            } else {
                                list.add(listItem);
                            }
                        }

                        output.put(property, List.of(Map.of(Keywords.LIST, list)));
//                                JsonProvider.instance().createObjectBuilder().add(Keywords.LIST, list)

                    } else if (JsonLdNode.isReference(item)) {

                        FramingState clonedState = new FramingState(state);
                        clonedState.setEmbedded(true);

                        Framing.with(
                                clonedState,
                                Arrays.asList(((Map<String, String>) item).get(Keywords.ID)),
                                Frame.of(subframe),
                                output,
                                property)
                                .ordered(ordered)
                                .frame();

                    } else if (JsonLdNode.isValueNode(item)) {
                        if (Frame.of(subframe).matchValue(item)) {
                            output.put(property, List.of(item));
                        }

                    } else {
                        output.put(property, item);
                    }
                }
            }

            // 4.7.4. - default values
            for (String property : frame.keys()) {
                if (output.containsKey(property)
                        || !Keywords.TYPE.equals(property) && Keywords.matchForm(property)
                        || Keywords.TYPE.equals(property) && !frame.isDefaultObject(property)) {
                    continue;
                }

                // 4.7.4.2.
                final Map<?, ?> propertyFrame;

                if (frame.get(property) instanceof Collection<?> array && !array.isEmpty()) {
                    propertyFrame = (Map<?, ?>) array.iterator().next();

                } else {
                    propertyFrame = Collections.emptyMap();
                }

                // 4.7.4.3.
                if (Frame.getBoolean(propertyFrame, Keywords.OMIT_DEFAULT, state.isOmitDefault())) {
                    continue;
                }

                // 4.7.4.4.
                var defaultValue = propertyFrame.get(Keywords.DEFAULT);

                if (defaultValue == null) {
                    defaultValue = Keywords.NULL;
                }

                output.put(property, Map.of(Keywords.PRESERVE, List.of(defaultValue)));
            }

            // 4.7.5. - reverse properties
            if (frame.contains(Keywords.REVERSE)) {

                final var reverseObject = frame.get(Keywords.REVERSE);

                if (reverseObject instanceof Map<?, ?> map) {

                    for (final var reverseEntry : map.entrySet()) {

                        final Frame subframe = Frame.of(map.get(reverseEntry.getValue()));

                        final Set<String> subjectProperties = state.getReversePropertySubjects(state.getGraphName(), (String) reverseEntry.getKey(), id);

                        if (!subjectProperties.isEmpty()) {

                            final var reverseResult = new LinkedHashMap<String, Object>();

                            final FramingState reverseState = new FramingState(state);
                            reverseState.setEmbedded(true);

                            Framing.with(
                                    reverseState,
                                    new ArrayList<>(subjectProperties),
                                    subframe,
                                    reverseResult,
                                    null)
                                    .ordered(ordered)
                                    .frame();

                            var reverse = (Map) output.get(Keywords.REVERSE);
                            
                            if (reverse == null) {
                                reverse = new LinkedHashMap<>();
                                output.put(Keywords.REVERSE, reverse);
                            }
                            
                            reverse.put(reverseEntry.getKey(), reverseResult.values());
//                                    .getMapBuilder(Keywords.REVERSE)
//                                    .add(reverseProperty, reverseResult.valuesToArray());
                        }
                    }
                }
            }

            state.removeLastParent();

            // 4.8.
            addToResult(parent, activeProperty, output);
        }
    }

    private static void addToResult(Map<String, Object> result, String property, Object value) {
        if (property == null) {
            result.put(Integer.toHexString(result.size()), value);

        } else {
            result.put(property, List.of(value)); // TODO ?!?!
        }
    }
}
