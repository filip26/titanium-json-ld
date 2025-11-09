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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.lang.Embed;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LdAdapter;
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

    private Framing(
            FramingState state,
            List<String> subjects,
            Frame frame,
            Map<String, Object> parent,
            String activeProperty) {
        this.state = state;
        this.subjects = subjects;
        this.frame = frame;
        this.parent = parent;
        this.activeProperty = activeProperty;

        // default values
        this.ordered = false;
    }

    public static final Framing with(
            FramingState state,
            List<String> subjects,
            Frame frame,
            Map<String, Object> parent,
            String activeProperty) {
        return new Framing(state, subjects, frame, parent, activeProperty);
    }

    public Framing ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public void frame() throws JsonLdException {

        // 2.
        final var embed = frame.getEmbed(state.getEmbed());

        final var isExplicit = frame.isExplicit(state.isExplicitInclusion());

        final var isRequireAll = frame.isRequireAll(state.isRequireAll());

        // 3.
        final var matchedSubjects = FrameMatcher
                .with(state, frame, isRequireAll)
                .match(subjects);

        // 4.
        final var ids = ordered
                ? matchedSubjects.stream().sorted().iterator()
                : matchedSubjects.iterator();

        while (ids.hasNext()) {

            final var id = ids.next();

            final var node = (Map<String, ?>) state.getGraphMap()
                    .find(state.getGraphName(), id)
                    .orElse(Map.of());

            final var nodeId = node.get(Keywords.ID) instanceof String stringNodeId
                    ? stringNodeId
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
                    && (Embed.NEVER == embed
                            || state.isParent(nodeId))) {
                addToResult(parent, activeProperty, output);
                continue;
            }

            // 4.4.
            if (state.isEmbedded()
                    && Embed.ONCE == embed
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
                            new ArrayList<>(
                                    state.getGraphMap().find(id)
                                            .or(() -> state.getGraphMap().find(state.getGraphName())) // <— fallback to current graph
                                            .map(Map::keySet)
                                            .orElse(Set.of())),
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
            final var unsortedProps = state.getGraphMap().properties(state.getGraphName(), id);

            final var properties = ordered
                    ? unsortedProps.stream().sorted().iterator()
                    : unsortedProps.iterator();

            while (properties.hasNext()) {

                final var property = properties.next();

                final var objects = state.getGraphMap().get(state.getGraphName(), id, property);

                // 4.7.1.
                if (Keywords.contains(property)) {
                    output.put(property, objects);
                    continue;
                }

                // 4.7.2.
                if (isExplicit && !frame.contains(property)) {
                    continue;
                }

                // 4.7.3.
                for (final var item : NativeAdapter.asCollection(objects)) {

                    var subframe = frame.get(property);

                    if (subframe == null) {
                        subframe = Map.of(Keywords.EMBED, "@".concat(embed.name().toLowerCase()),
                                Keywords.EXPLICIT, isExplicit,
                                Keywords.REQUIRE_ALL, isRequireAll);
                    }

                    // 4.7.3.1.
                    if (item instanceof Map itemMap && LdAdapter.isList(itemMap)) {

                        Object listFrameValue = null;

                        var propertyValue = frame.get(property);

                        if (propertyValue instanceof Collection array
                                && !array.isEmpty()
                                && array.iterator().next() instanceof Map map) {
                            listFrameValue = map.get(Keywords.LIST);
                        }

                        if (listFrameValue == null) {
                            listFrameValue = Map.of(
                                    Keywords.EMBED, "@".concat(embed.name().toLowerCase()),
                                    Keywords.EXPLICIT, isExplicit,
                                    Keywords.REQUIRE_ALL, isRequireAll);
                        }

                        final Frame listFrame = Frame.of(listFrameValue);

                        final var list = new ArrayList<Object>();

                        for (final var listItem : NativeAdapter.asCollection(itemMap.get(Keywords.LIST))) {

                            // 4.7.3.1.1.
                            if (LdAdapter.isReference(listItem)) {

                                FramingState listState = new FramingState(state);
                                listState.setEmbedded(true);

                                final var listResult = new LinkedHashMap<String, Object>();

                                @SuppressWarnings("unchecked")
                                final var idMap = ((Map<String, String>) listItem).get(Keywords.ID);

                                Framing.with(
                                        listState,
                                        Arrays.asList(idMap),
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

                        LdAdapter.setOrAdd(output, property, Map.of(Keywords.LIST, list));

                    } else if (LdAdapter.isReference(item)) {

                        FramingState clonedState = new FramingState(state);
                        clonedState.setEmbedded(true);

                        @SuppressWarnings("unchecked")
                        final var idMap = ((Map<String, String>) item).get(Keywords.ID);

                        Framing.with(
                                clonedState,
                                Arrays.asList(idMap),
                                Frame.of(subframe),
                                output,
                                property)
                                .ordered(ordered)
                                .frame();

                    } else if (item instanceof Map itemMap && LdAdapter.isValueNode(itemMap)) {

                        if (Frame.of(subframe).matchValue(item)) {
                            LdAdapter.setOrAdd(output, property, item);
                        }

                    } else {
                        LdAdapter.setOrAdd(output, property, item);
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
                    propertyFrame = Map.of();
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

                LdAdapter.setOrAdd(output, property, Map.of(Keywords.PRESERVE, defaultValue));
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

                            @SuppressWarnings("unchecked")
                            var reverse = (Map<String, Object>) output.get(Keywords.REVERSE);

                            if (reverse == null) {
                                reverse = new LinkedHashMap<String, Object>();
                                output.put(Keywords.REVERSE, reverse);
                            }

                            LdAdapter.setOrAdd(
                                    reverse,
                                    (String) reverseEntry.getKey(),
                                    reverseResult.values());
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
            return;
        }
        LdAdapter.setOrAdd(result, property, value);
    }
}
