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
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdAdapter;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.java.NativeAdapter;

public final class FrameMatcher {

    // required
    private FramingState state;
    private Frame frame;
    private boolean requireAll;

    private FrameMatcher(FramingState state, Frame frame, boolean requireAll) {
        this.state = state;
        this.frame = frame;
        this.requireAll = requireAll;
    }

    public static final FrameMatcher with(FramingState state, Frame frame, boolean requireAll) {
        return new FrameMatcher(state, frame, requireAll);
    }

    public List<String> match(final Collection<String> subjects) throws JsonLdError {

        // 1. if frame is empty then all subject match
        if (frame.isWildCard()) {
            return new ArrayList<>(subjects);
        }

        final var result = new ArrayList<String>();

        for (final var subject : subjects) {

            if (match((Map<?, ?>) state.getGraphMap()
                    .find(state.getGraphName(), subject)
                    .orElse(Map.of()))) {
                result.add(subject);
            }
        }

        return result;
    }

    public boolean match(final Map<?, ?> node) throws JsonLdError {

        int count = 0;

        boolean nonKeywordProperty = false;

        for (final String property : frame.keys()) {

            var nodeValue = node.get(property);

            // 2.1.
            if (Keywords.ID.equals(property)) {

                nodeValue = nodeValue instanceof Collection
                        ? nodeValue
                        : List.of(nodeValue);

                final var propertyValue = frame.get(property);

                if (propertyValue instanceof Collection<?> array
                        && array.stream().anyMatch(((Collection<?>) nodeValue)::contains)
                        || frame.isWildCard(Keywords.ID)
                        || frame.isNone(Keywords.ID)) {

                    if (requireAll) {
                        count++;
                        continue;
                    }
                    return true;
                }
                return false;

                // 2.2.
            } else if (Keywords.TYPE.equals(property)) {

                if ((nodeValue != null && !((Collection<?>) nodeValue).isEmpty() && frame.isWildCard(property))
                        || ((nodeValue == null || ((Collection<?>) nodeValue).isEmpty()) && frame.isNone(property))
                        || frame.isDefaultObject(property)
                        || (nodeValue != null && frame.asCollection(property).stream().anyMatch(((Collection<?>) nodeValue)::contains))) {

                    if (requireAll) {
                        count++;
                        continue;
                    }
                    return true;
                }
                return false;

                // skip other keywords
            } else if (Keywords.matchForm(property)) {
                continue;
            }

            nonKeywordProperty = true;

            final var propertyValue = frame.get(property);

            final var propertyFrame = propertyValue instanceof Collection array && !array.isEmpty()
                    ? Frame.of(array)
                    : null;

            final var nodeValues = nodeValue instanceof Collection<?> array
                    ? array
                    : nodeValue != null
                            ? List.of(nodeValue)
                            : List.of();

            // 2.5.
            if (nodeValues.isEmpty()
                    && propertyFrame != null
                    && propertyFrame.containsOnly(Keywords.DEFAULT)) {
                continue;
            }

            // 2.6.
            if (nodeValues.isEmpty() && frame.isNone(property)) {

                if (requireAll) {
                    count++;
                    continue;
                }
                return true;

            } else if (!nodeValues.isEmpty() && frame.isNone(property)) {
                return false;
            }

            // 2.7.
            if (!nodeValues.isEmpty() && propertyFrame != null && propertyFrame.isWildCard()) {

                if (requireAll) {
                    count++;
                    continue;
                }
                return true;
            }

            if (propertyFrame == null) {

                if (nodeValues.isEmpty()) {
                    if (requireAll) {
                        count++;
                        continue;
                    }
                    return true;
                }

            } else {

                if (propertyFrame.isList()) {

                    var listValue = propertyFrame.get(Keywords.LIST);

                    if (!nodeValues.isEmpty() 
                            && nodeValues.iterator().next() instanceof Map nodeValueMap
                            && JsonLdAdapter.isList(nodeValueMap)) {

                        final var first = (Map<?, ?>) nodeValues.iterator().next();

                        final var nodeListValue = first.get(Keywords.LIST);

                        if (((Collection<?>) listValue).iterator().next() instanceof Map map
                                && JsonLdAdapter.isValueNode(map)) {

                            final Frame frame = Frame.of(listValue);
                            boolean match = false;

                            for (final var value : NativeAdapter.asCollection(nodeListValue)) {

                                match = frame.matchValue(value);
                                if (match) {
                                    break;
                                }
                            }

                            if (match) {
                                if (requireAll) {
                                    count++;
                                    continue;
                                }
                                return true;
                            }

                        } else if (JsonLdAdapter.isNode(((Collection<?>) listValue).iterator().next())
                                || JsonLdAdapter.isReference(((Collection<?>) listValue).iterator().next())) {

                            final Frame frame = Frame.of(listValue);
                            boolean match = false;

                            for (final var value : NativeAdapter.asCollection(nodeListValue)) {

                                match = frame.matchNode(state, value, requireAll);

                                if (match) {
                                    break;
                                }
                            }

                            if (match) {
                                if (requireAll) {
                                    count++;
                                    continue;
                                }
                                return true;
                            }
                        }
                    }

                } else if (propertyFrame.isValuePattern()) {

                    if (nodeValues.stream().anyMatch(propertyFrame::matchValue)) {
                        if (requireAll) {
                            count++;
                            continue;
                        }
                        return true;
                    }

                } else if (propertyFrame.isReference()) {

                    boolean match = false;

                    if (!nodeValues.isEmpty()) {

                        for (final var values : nodeValues) {

                            match = propertyFrame.matchNode(state, values, requireAll);
                            if (match) {
                                break;
                            }
                        }
                    }

                    if (match) {
                        if (requireAll) {
                            count++;
                            continue;
                        }
                        return true;
                    }

                } else {

                    if (!nodeValues.isEmpty()) {
                        if (requireAll) {
                            count++;
                            continue;
                        }
                        return true;
                    }
                }
            }

            if (requireAll) {
                return false;
            }
        }

        return !nonKeywordProperty || count > 0;
    }
}
