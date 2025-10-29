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

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.TreeDocument;
import com.apicatalog.jsonld.lang.Embed;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.tree.io.PolyNode;

public final class Frame {

    public static final Frame EMPTY = new Frame(Map.of(), null, null, null);

    private final Map<String, ?> frameNode;
    private final PolyNode context;

    private final URI contextUrl;
    private final URI documentUrl;

    private Frame(
            final Map<String, ?> frameNode,
            final PolyNode context,
            final URI contextUrl,
            final URI documentUrl) {
        this.frameNode = frameNode;
        this.context = context;
        this.contextUrl = contextUrl;
        this.documentUrl = documentUrl;
    }

    public static final Frame of(final TreeDocument document, final JsonLdOptions options) throws JsonLdError, IOException {

//      final JsonStructure frameStructure;
//
//      if (frameDocument instanceof JsonDocument x) {
//          frameStructure = x.getJsonContent()
//                  .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but null."));
//      } else {
//          throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but null.");
//      }

//      if (JsonUtils.isNotObject(frameStructure)) {
//          throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but [" + frameStructure + "].");
//      }
//
//      final JsonObject frameObject = frameStructure.asJsonObject();

//        JsonValue context = JsonValue.EMPTY_JSON_OBJECT;
//
//        if (frameObject.containsKey(Keywords.CONTEXT)) {
//            context = frameObject.get(Keywords.CONTEXT);
//        }

        final var node = document.content().node();
        final var adapter = document.content().adapter();

        if (!adapter.isMap(node)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but [" + node + "].");
        }

        PolyNode context = null;

        if (adapter.keys(node).contains(Keywords.CONTEXT)) {
            var contextNode = adapter.property(Keywords.CONTEXT, node);
            if ((adapter.isString(contextNode)
                    || adapter.isCollection(contextNode)
                    || adapter.isMap(contextNode))
                    && !adapter.isEmptyCollection(contextNode)
                    && !adapter.isEmptyMap(contextNode)) {
                context = new PolyNode(contextNode, adapter);
            }
        }

        var expanded = Expander.expand(
                document,
                true,
                new JsonLdOptions(options).setOrdered(false));

        return of(expanded, context, document.contextUrl(), document.documentUrl());
    }

    static final Frame of(final Object expanded) throws JsonLdError {
        // TODO
        return of(expanded, null, null, null);
    }

    static final Frame of(
            final Object expanded,
            final PolyNode context,
            final URI contextUrl,
            final URI documentUrl) throws JsonLdError {

        final Map<String, ?> frameMap;

        // 1.
        if (expanded instanceof Collection array) {

            if (array.size() == 1 && (array.iterator().next() instanceof Map map)) {
                frameMap = map;

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object nor an array containing JSON object [" + expanded + "]");
            }

        } else if ((expanded instanceof Map map)) {
            frameMap = map;

        } else if (expanded == null) {
            return EMPTY;

        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object. [" + expanded + "]");
        }

        // 1.2.
        if (frameMap.containsKey(Keywords.ID) && !validateFrameId(frameMap)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @id value is not valid [@id = " + frameMap.get(Keywords.ID) + "].");
        }

        // 1.3.
        if (frameMap.containsKey(Keywords.TYPE) && !validateFrameType(frameMap)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_FRAME, "Frame @type value is not valid [@type = " + frameMap.get(Keywords.TYPE) + "].");
        }

        return new Frame(frameMap, context, contextUrl, documentUrl);
    }

    public Embed getEmbed(final Embed defaultValue) throws JsonLdError {

        if (frameNode.containsKey(Keywords.EMBED)) {

            var embed = frameNode.get(Keywords.EMBED);

            if (embed == null) {
                return defaultValue;
            }

            if (embed instanceof Map map && JsonLdAdapter.isValueNode(map)) {
                embed = JsonLdAdapter.findValue(embed).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE));
            }

            if (embed instanceof String stringValue) {

                if (Keywords.noneMatch(stringValue, Keywords.ALWAYS, Keywords.ONCE, Keywords.NEVER)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + stringValue + "].");
                }

                return Embed.valueOf(stringValue.substring(1).toUpperCase());

            } else if (Boolean.FALSE.equals(embed)) {
                return Embed.NEVER;

            } else if (Boolean.TRUE.equals(embed)) {
                return Embed.ONCE;
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

            if (value instanceof Map map && JsonLdAdapter.isValueNode(map)) {
                value = JsonLdAdapter.findValue(value).orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_FRAME));
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

    private static final boolean validateFrameId(Map<?, ?> frame) {

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

    private static final boolean validateFrameType(Map<?, ?> frame) {

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
                || type instanceof String stringType && (Keywords.JSON.equals(stringType) // see https://github.com/w3c/json-ld-framing/issues/142
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
                        ? List.of(value)
                        : List.of();
    }

    @Override
    public String toString() {
        return frameNode.toString();
    }

    public boolean isValuePattern() {
        return JsonLdAdapter.isValueNode(frameNode);
    }

    public boolean matchValue(Object value) {
        return value instanceof Map map && ValuePatternMatcher.match(frameNode, map);
    }

    public boolean isDefaultObject(String property) {
        return JsonLdAdapter.isDefault(frameNode.get(property))
                || frameNode.get(property) instanceof Collection array
                        && array.size() == 1
                        && JsonLdAdapter.isDefault(array.iterator().next());
    }

    public boolean isPattern() {
        return JsonLdAdapter.isNode(frameNode);
    }

    public boolean isReference() {
        return JsonLdAdapter.isReference(frameNode);
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
        return JsonLdAdapter.isList(frameNode);
    }

    public PolyNode context() {
        return context;
    }

    public boolean isDefault(String graphKey) {
//        if (context != null) {
        // FIXME frameObject
        for (final String key : frameNode.keySet()) {
            if (key.equals(graphKey)) {
                return true;
            }
        }
//        }
        return false;
    }

    public boolean hasContext() {
        return context != null;
    }

    public URI getContextUrl() {
        return contextUrl;
    }

    public URI getDocumentUrl() {
        return documentUrl;
    }
}
