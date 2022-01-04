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
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdEmbed;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DefaultObject;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public final class Frame {

    public static final Frame EMPTY = new Frame(JsonValue.EMPTY_JSON_OBJECT);

    private final JsonObject frameObject;

    private Frame(final JsonObject frameObject) {
        this.frameObject = frameObject;
    }

    public static final Frame of(final JsonStructure structure) throws JsonLdError {

        final JsonObject frameObject;

        // 1.
        if (JsonUtils.isArray(structure)) {

            if (structure.asJsonArray().size() != 1
                    || JsonUtils.isNotObject(structure.asJsonArray().get(0))
                    ) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object nor an array containing JSON object [" + structure + "]");
            }

            frameObject = structure.asJsonArray().getJsonObject(0);


        } else if (JsonUtils.isObject(structure)) {

            frameObject = structure.asJsonObject();

        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object. [" + structure + "]");
        }

        // 1.2.
        if (frameObject.containsKey(Keywords.ID) && !validateFrameId(frameObject)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @id value is not valid [@id = " + frameObject.get(Keywords.ID) + "].");
        }

        // 1.3.
        if (frameObject.containsKey(Keywords.TYPE) && !validateFrameType(frameObject)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @type value i not valid [@type = " + frameObject.get(Keywords.TYPE) + "].");
        }
        return new Frame(frameObject);
    }

    public JsonLdEmbed getEmbed(final JsonLdEmbed defaultValue) throws JsonLdError {

        if (frameObject.containsKey(Keywords.EMBED)) {

            JsonValue embed = frameObject.get(Keywords.EMBED);

            if (JsonUtils.isNull(embed)) {
                return defaultValue;
            }

            if (ValueObject.isValueObject(embed)) {
                embed = ValueObject.getValue(embed).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE));
            }

            if (JsonUtils.isString(embed)) {

                String stringValue = ((JsonString)embed).getString();

                if (Keywords.noneMatch(stringValue, Keywords.ALWAYS, Keywords.ONCE, Keywords.NEVER)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + stringValue + "].");
                }

                return JsonLdEmbed.valueOf(stringValue.substring(1).toUpperCase());

            } else if (JsonUtils.isFalse(embed)) {
                return JsonLdEmbed.NEVER;

            } else if (JsonUtils.isTrue(embed)) {
                return JsonLdEmbed.ONCE;
            }

            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + embed + "].");
         }

        return defaultValue;
    }

    public boolean getExplicit(boolean defaultValue) throws JsonLdError {
        return getBoolean(frameObject, Keywords.EXPLICIT, defaultValue);
    }

    public boolean getRequireAll(boolean defaultValue) throws JsonLdError {
        return getBoolean(frameObject, Keywords.REQUIRE_ALL, defaultValue);
    }

    public static final boolean getBoolean(JsonObject frame, String key, boolean defaultValue) throws JsonLdError {

        if (frame.containsKey(key)) {

            JsonValue value = frame.get(key);

            if (JsonUtils.isNull(value)) {
                return defaultValue;
            }

            if (ValueObject.isValueObject(value)) {
                value = ValueObject.getValue(value).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_FRAME));
            }

            if (JsonUtils.isString(value)) {
                if ("true".equalsIgnoreCase(((JsonString)value).getString())) {
                    return true;

                } else if ("false".equalsIgnoreCase(((JsonString)value).getString())) {
                    return false;
                }
            }

            if (JsonUtils.isNotBoolean(value)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME);
            }

            return JsonUtils.isTrue(value);
        }
        return defaultValue;
    }

    private static final boolean validateFrameId(JsonObject frame) {

        final JsonValue id = frame.get(Keywords.ID);

        if (JsonUtils.isNonEmptyArray(id)) {

            final JsonArray idArray = id.asJsonArray();

            return ((idArray.size() == 1 && JsonUtils.isEmptyObject(idArray.get(0)))
                    || idArray
                        .stream()
                        .noneMatch(item -> JsonUtils.isNotString(item)
                                            || UriUtils.isNotAbsoluteUri(((JsonString)item).getString(), true)));
        }
        return JsonUtils.isString(id) && UriUtils.isAbsoluteUri(((JsonString)id).getString(), true);
    }

    private static final boolean validateFrameType(JsonObject frame) {

        final JsonValue type = frame.get(Keywords.TYPE);

        if (JsonUtils.isNonEmptyArray(type)) {

            final JsonArray typeArray = type.asJsonArray();

            return ((typeArray.size() == 1
                        && (JsonUtils.isEmptyObject(typeArray.get(0))
                            || (JsonUtils.isObject(typeArray.get(0))
                                    && typeArray.get(0).asJsonObject().containsKey(Keywords.DEFAULT)
                                    )
                            )
                    )
                    || typeArray
                        .stream()
                        .noneMatch(item -> JsonUtils.isNotString(item)
                                            || UriUtils.isNotAbsoluteUri(((JsonString)item).getString(), true)));
        }

        return JsonUtils.isEmptyArray(type)
                || JsonUtils.isEmptyObject(type)
                || JsonUtils.isString(type) && UriUtils.isAbsoluteUri(((JsonString)type).getString(), true);
    }

    public Set<String> keys() {
        return frameObject.keySet();
    }

    public JsonValue get(String property) {
        return frameObject.get(property);
    }

    public boolean contains(String property) {
        return frameObject.containsKey(property);
    }

    public boolean containsOnly(String property) {
        return frameObject.containsKey(property) && ValuePatternMatcher.isWildcard(frameObject, property);
    }

    public boolean isWildCard() {
        return ValuePatternMatcher.isWildcard(frameObject);
    }

    public boolean isWildCard(String property) {
        return frameObject.containsKey(property)
                    && ValuePatternMatcher.isWildcard(frameObject.get(property));
    }

    public boolean isNone(String property) {
        return frameObject.containsKey(property)
                    && ValuePatternMatcher.isNone(frameObject.get(property));
    }

    public Collection<JsonValue> getCollection(String property) {
        return frameObject.containsKey(property)
                    ? JsonUtils.toJsonArray(frameObject.get(property))
                    : JsonValue.EMPTY_JSON_ARRAY;
    }

    @Override
    public String toString() {
        return frameObject.toString();
    }

    public boolean isValuePattern() {
        return ValueObject.isValueObject(frameObject);
    }

    public boolean matchValue(JsonValue value) {
        return JsonUtils.isObject(value) && ValuePatternMatcher.with(frameObject, value.asJsonObject()).match();
    }

    public boolean isDefaultObject(String property) {
        return DefaultObject.isDefaultObject(frameObject.get(property))
                || JsonUtils.isArray(frameObject.get(property))
                    && frameObject.get(property).asJsonArray().size() == 1
                    && DefaultObject.isDefaultObject(frameObject.get(property).asJsonArray().get(0))
                ;
    }

    public boolean isNodePattern() {
        return NodeObject.isNodeObject(frameObject);
    }

    public boolean isNodeReference() {
        return NodeObject.isNodeReference(frameObject);
    }

    public boolean matchNode(FramingState state, JsonValue value, boolean requireAll) throws JsonLdError {

        if (JsonUtils.isNotObject(value) || !value.asJsonObject().containsKey(Keywords.ID)) {
            return false;
        }

        final Optional<Map<String, JsonValue>> valueObject =
                    state
                        .getGraphMap()
                        .get(state.getGraphName())
                        .map(graph -> graph.get(value.asJsonObject().getString(Keywords.ID)));

        return valueObject.isPresent() && FrameMatcher.with(state, this, requireAll).match(valueObject.get());
    }

    public boolean isListObject() {
        return ListObject.isListObject(frameObject);
    }
}
