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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdEmbed;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.lang.JsonLdNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;

public final class Frame {

    public static final Frame EMPTY = new Frame(Collections.emptyMap());

    private final Map<String, ?> frameNode;

    private Frame(final Map<String, ?> frameObject) {
        this.frameNode = frameObject;
    }

    public static final Frame of(final Object node) throws JsonLdError {

        final Map<String, ?> frameMap;

        // 1.
        if (node instanceof Collection<?> array) {

            if (array.size() == 1 && (array.iterator().next() instanceof Map map)) {
                frameMap = map;

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object nor an array containing JSON object [" + node + "]");
            }

        } else if (node instanceof Map map) {
            frameMap = map;

        } else if (node == null) {
            return EMPTY;   //TODO ?!?!
            
        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object. [" + node + "]");
        }

        // 1.2.
        if (frameMap.containsKey(Keywords.ID) && !validateFrameId(frameMap)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @id value is not valid [@id = " + frameMap.get(Keywords.ID) + "].");
        }

        // 1.3.
        if (frameMap.containsKey(Keywords.TYPE) && !validateFrameType(frameMap)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @type value is not valid [@type = " + frameMap.get(Keywords.TYPE) + "].");
        }
        
        return new Frame(frameMap);
    }

    public JsonLdEmbed getEmbed(final JsonLdEmbed defaultValue) throws JsonLdError {

        if (frameNode.containsKey(Keywords.EMBED)) {

            var embed = frameNode.get(Keywords.EMBED);

            if (embed == null) {
                return defaultValue;
            }

            if (JsonLdNode.isValueNode(embed)) {
                embed = JsonLdNode.findValue(embed).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE));
            }

            if (embed instanceof String stringValue) {

                if (Keywords.noneMatch(stringValue, Keywords.ALWAYS, Keywords.ONCE, Keywords.NEVER)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + stringValue + "].");
                }

                return JsonLdEmbed.valueOf(stringValue.substring(1).toUpperCase());

            } else if (Boolean.FALSE.equals(embed)) {
                return JsonLdEmbed.NEVER;

            } else if (Boolean.TRUE.equals(embed)) {
                return JsonLdEmbed.ONCE;
            }

            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + embed + "].");
        }

        return defaultValue;
    }

    public boolean getExplicit(boolean defaultValue) throws JsonLdError {
        return getBoolean(frameNode, Keywords.EXPLICIT, defaultValue);
    }

    public boolean getRequireAll(boolean defaultValue) throws JsonLdError {
        return getBoolean(frameNode, Keywords.REQUIRE_ALL, defaultValue);
    }

    public static final boolean getBoolean(Map<?, ?> frame, String key, boolean defaultValue) throws JsonLdError {

        if (frame.containsKey(key)) {

            var value = frame.get(key);

            if (value == null) {
                return defaultValue;
            }

            if (JsonLdNode.isValueNode(value)) {
                value = JsonLdNode.findValue(value).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_FRAME));
            }

            if (value instanceof String bool) {
                if ("true".equalsIgnoreCase(bool)) {
                    return true;

                } else if ("false".equalsIgnoreCase(bool)) {
                    return false;
                }
            }

            if (value instanceof Boolean bool) {
                return bool;
            }
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
        }
        return defaultValue;
    }

    private static final boolean validateFrameId(Map<String, ?> frame) {

        final var id = frame.get(Keywords.ID);

        if (id instanceof Collection<?> idArray && !idArray.isEmpty()) {
            return ((idArray.size() == 1
                    && idArray.iterator().next() instanceof Map map && map.isEmpty())
                    || idArray
                            .stream()
                            .noneMatch(item -> !(item instanceof String uri)
                                    || UriUtils.isNotAbsoluteUri(uri, UriValidationPolicy.Full)));
        }
        return id instanceof String uri
                && UriUtils.isAbsoluteUri(uri, UriValidationPolicy.Full);
    }

    private static final boolean validateFrameType(Map<String, ?> frame) {

        var type = frame.get(Keywords.TYPE);
        
        if (type instanceof Collection<?> typeArray && !typeArray.isEmpty()) {
            return ((typeArray.size() == 1
                    && (typeArray.iterator().next() instanceof Map map &&
                            (map.isEmpty()
                                    || map.containsKey(Keywords.DEFAULT)
                            // JsonUtils.containsKey(typeArray.get(0), Keywords.DEFAULT)
                            )))
                    || typeArray
                            .stream()
                            .noneMatch(item -> !(item instanceof String uri)
                                    || UriUtils.isNotAbsoluteUri(uri, UriValidationPolicy.Full)));
        }

        return type instanceof Collection array && array.isEmpty()
                || type instanceof Map map && map.isEmpty()
                || type instanceof String stringType && (
                        Keywords.JSON.equals(stringType)    // see https://github.com/w3c/json-ld-framing/issues/142
                        || UriUtils.isAbsoluteUri(stringType, UriValidationPolicy.Full));
    }

    public Set<String> keys() {
        return frameNode.keySet();
    }

    public Object get(String property) {
        return frameNode.get(property);
    }

    public boolean contains(String property) {
        return frameNode.containsKey(property);
    }

    public boolean containsOnly(String property) {
        return frameNode.containsKey(property) && ValuePatternMatcher.isWildcard(frameNode, property);
    }

    public boolean isWildCard() {
        return ValuePatternMatcher.isWildcard(frameNode);
    }

    public boolean isWildCard(String property) {
        return frameNode.containsKey(property)
                && ValuePatternMatcher.isWildcard(frameNode.get(property));
    }

    public boolean isNone(String property) {
        return frameNode.containsKey(property)
                && ValuePatternMatcher.isNone(frameNode.get(property));
    }

    public Collection<?> asCollection(String property) {

        final var value = frameNode.get(property);

        return value instanceof Collection col
                ? col
                : value != null
                        ? Set.of(value)
                        : Collections.emptyList();
    }

    @Override
    public String toString() {
        return frameNode.toString();
    }

    public boolean isValuePattern() {
        return JsonLdNode.isValueNode(frameNode);
    }

    public boolean matchValue(Object value) {
        return value instanceof Map map && ValuePatternMatcher.with(frameNode, map).match();
//        return JsonUtils.isObject(value) && ValuePatternMatcher.with(frameObject, value.asJsonObject()).match();
    }

    public boolean isDefaultObject(String property) {
        return JsonLdNode.isDefault(frameNode.get(property))
                || frameNode.get(property) instanceof Collection array
                        && array.size() == 1
                        && JsonLdNode.isDefault(array.iterator().next());
    }

    public boolean isPattern() {
        return JsonLdNode.isNode(frameNode);
    }

    public boolean isReference() {
        return JsonLdNode.isReference(frameNode);
    }

    public boolean matchNode(FramingState state, Object value, boolean requireAll) throws JsonLdError {

        if (value instanceof Map map && map.containsKey(Keywords.ID)) {

            final var valueObject = state
                    .getGraphMap()
                    .find(state.getGraphName())
                    .map(graph -> (Map<String, ?>) graph.get((String) map.get(Keywords.ID)));

            return valueObject.isPresent()
                    && FrameMatcher.with(state, this, requireAll)
                            .match(valueObject.get());
        }
        return false;
    }

    public boolean isList() {
        return JsonLdNode.isList(frameNode);
    }
}
