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
package no.hasmac.jsonld.framing;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.ListObject;
import no.hasmac.jsonld.lang.NodeObject;
import no.hasmac.jsonld.lang.ValueObject;

import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

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

    public static FrameMatcher with(FramingState state, Frame frame, boolean requireAll) {
        return new FrameMatcher(state, frame, requireAll);
    }

    public List<String> match(final Collection<String> subjects) throws JsonLdError {

        // 1. if frame is empty then all subject match
        if (frame.isWildCard()) {
            return new ArrayList<>(subjects);
        }

        final List<String> result = new ArrayList<>();

        for (final String subject : subjects) {

            if (match(state.getGraphMap().get(state.getGraphName(), subject))) {
                result.add(subject);
            }
        }

        return result;
    }

    public boolean match(final Map<String, JsonValue> node) throws JsonLdError {

        int count = 0;

        boolean nonKeywordProperty = false;

        for (final String property : frame.keys()) {

            JsonValue nodeValue = node.get(property);

            // 2.1.
            if (Keywords.ID.equals(property)) {

                nodeValue = JsonUtils.toJsonArray(nodeValue);

                if (JsonUtils.toStream(frame.get(property)).anyMatch(nodeValue.asJsonArray()::contains)
                        || frame.isWildCard(Keywords.ID)
                        || frame.isNone(Keywords.ID)
                        ) {

                    if (requireAll) {
                        count++;
                        continue;
                    }
                    return true;
                }
                return false;

            // 2.2.
            } else if (Keywords.TYPE.equals(property)) {

                if ((JsonUtils.isNotNull(nodeValue) && !nodeValue.asJsonArray().isEmpty() && frame.isWildCard(property))
                        || ((JsonUtils.isNull(nodeValue) || nodeValue.asJsonArray().isEmpty()) && frame.isNone(property))
                        || frame.isDefaultObject(property)
                        || (JsonUtils.isNotNull(nodeValue) && frame.getCollection(property).stream().anyMatch(nodeValue.asJsonArray()::contains))
                        ){

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

            JsonValue propertyValue = frame.get(property);

            final Frame propertyFrame =
                            (JsonUtils.isNotNull(propertyValue) && JsonUtils.isNonEmptyArray(propertyValue))
                                ? Frame.of((JsonStructure)propertyValue)
                                : null;

            final JsonArray nodeValues = nodeValue != null
                                            ? JsonUtils.toJsonArray(nodeValue)
                                            : JsonValue.EMPTY_JSON_ARRAY;

            // 2.5.
            if (nodeValues.isEmpty()
                    && propertyFrame != null
                    && propertyFrame.containsOnly(Keywords.DEFAULT)
                    ) {
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

            } else  {

                if (propertyFrame.isListObject()) {

                    JsonValue listValue = propertyFrame.get(Keywords.LIST);

                    if (!nodeValues.isEmpty() && ListObject.isListObject(nodeValues.get(0))) {

                        JsonValue nodeListValue = nodeValues.get(0).asJsonObject().get(Keywords.LIST);

                        if (ValueObject.isValueObject(listValue.asJsonArray().get(0))) {

                            final Frame frame = Frame.of((JsonStructure)listValue);
                            boolean match = false;

                            for (final JsonValue value : JsonUtils.toCollection(nodeListValue)) {

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

                        } else if (NodeObject.isNodeObject(listValue.asJsonArray().get(0)) || NodeObject.isNodeReference(listValue.asJsonArray().get(0))) {

                            final Frame frame = Frame.of((JsonStructure)listValue);
                            boolean match = false;

                            for (final JsonValue value : JsonUtils.toCollection(nodeListValue)) {

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

                } else if (propertyFrame.isNodeReference()) {

                        boolean match = false;

                        if (!nodeValues.isEmpty()) {

                            for (JsonValue values : nodeValues) {

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

                } else  {

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
