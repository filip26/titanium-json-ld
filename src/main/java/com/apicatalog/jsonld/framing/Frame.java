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

import com.apicatalog.jsonld.JsonLdAdapter;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.lang.Embed;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.uri.UriUtils;
import com.apicatalog.web.uri.UriValidationPolicy;

public final class Frame {

    public static final Frame EMPTY = new Frame(Map.of());

    private final Map<String, ?> expanded;

    private Frame(final Map<String, ?> expanded) {
        this.expanded = expanded;
    }

    public static final Frame of(final Document frame, final JsonLdOptions options) throws JsonLdException, IOException {

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

        var expanded = Expander.expandFrame(
                frame,
                new JsonLdOptions(options).setOrdered(false));

        return of(expanded);// , context, document.contextUrl(), document.documentUrl());
    }

//    static final Frame of(final Object expanded) throws JsonLdException {
//        // TODO
//        return of(expanded, null, null, null);
//    }
    
    public static final URI contextBase(final Document frame, final JsonLdOptions options) {
        return (frame.contextUrl() != null)
                ? frame.documentUrl()
                : options.getBase();

    }

    public static final PolyNode contextNode(final PolyNode frame) throws JsonLdException {

        final var node = frame.node();
        final var adapter = frame.adapter();

        if (!adapter.isMap(node)) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Frame is not JSON object but [" + node + "].");
        }

        final var context = adapter.property(Keywords.CONTEXT, node);

        if (context != null) {
            return new PolyNode(context, adapter);
        }
        
//        if (context != null
//                && (adapter.isString(context)
//                        || adapter.isCollection(context)
//                        || adapter.isMap(context))
//                && !adapter.isEmptyCollection(context)
//                && !adapter.isEmptyMap(context)) {
//            return new PolyNode(context, adapter);
//        }

        return new PolyNode(Map.of(), NativeAdapter.instance());
    }

    static final Frame of(
            final Object expanded
//            final PolyNode context
//            final URI contextUrl,
//            final URI documentUrl
//            
    ) throws JsonLdException {

        final Map<String, Object> frameMap;

        // 1.
        if (expanded instanceof Collection array) {

            if (array.size() == 1 && (array.iterator().next() instanceof Map map)) {

                @SuppressWarnings("unchecked")
                var typedMap = (Map<String, Object>) map;
                frameMap = typedMap;

            } else {
                throw new JsonLdException(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object nor an array containing JSON object [" + expanded + "]");
            }

        } else if ((expanded instanceof Map map)) {

            @SuppressWarnings("unchecked")
            var typedMap = (Map<String, Object>) map;
            frameMap = typedMap;

        } else if (expanded == null) {
            return EMPTY;

        } else {
            throw new JsonLdException(JsonLdErrorCode.INVALID_FRAME, "Frame is not JSON object. [" + expanded + "]");
        }

        // 1.2.
        if (frameMap.containsKey(Keywords.ID) && !validateFrameId(frameMap)) {
            throw new JsonLdException(JsonLdErrorCode.INVALID_FRAME, "Frame @id value is not valid [@id = " + frameMap.get(Keywords.ID) + "].");
        }

        // 1.3.
        if (frameMap.containsKey(Keywords.TYPE) && !validateFrameType(frameMap)) {
            throw new JsonLdException(JsonLdErrorCode.INVALID_FRAME, "Frame @type value is not valid [@type = " + frameMap.get(Keywords.TYPE) + "].");
        }

        return new Frame(frameMap);// , context, contextUrl, documentUrl);
    }

    public Embed getEmbed(final Embed defaultValue) throws JsonLdException {

        if (expanded.containsKey(Keywords.EMBED)) {

            var embed = expanded.get(Keywords.EMBED);

            if (embed == null) {
                return defaultValue;
            }

            if (embed instanceof Map map && JsonLdAdapter.isValueNode(map)) {
                embed = JsonLdAdapter.findValue(embed).orElseThrow(() -> new JsonLdException(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE));
            }

            if (embed instanceof String stringValue) {

                if (Keywords.noneMatch(stringValue, Keywords.ALWAYS, Keywords.ONCE, Keywords.NEVER)) {
                    throw new JsonLdException(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + stringValue + "].");
                }

                return Embed.valueOf(stringValue.substring(1).toUpperCase());

            } else if (Boolean.FALSE.equals(embed)) {
                return Embed.NEVER;

            } else if (Boolean.TRUE.equals(embed)) {
                return Embed.ONCE;
            }

            throw new JsonLdException(JsonLdErrorCode.INVALID_KEYWORD_EMBED_VALUE, "The value for @embed is not one recognized for the object embed flag [@embed = " + embed + "].");
        }

        return defaultValue;
    }

    public boolean isExplicit(boolean defaultValue) throws JsonLdException {
        return getBoolean(expanded, Keywords.EXPLICIT, defaultValue);
    }

    public boolean isRequireAll(boolean defaultValue) throws JsonLdException {
        return getBoolean(expanded, Keywords.REQUIRE_ALL, defaultValue);
    }

    public static final boolean getBoolean(Map<?, ?> frame, String key, boolean defaultValue) throws JsonLdException {

        if (frame.containsKey(key)) {

            var value = frame.get(key);

            if (value == null) {
                return defaultValue;
            }

            if (value instanceof Map map && JsonLdAdapter.isValueNode(map)) {
                value = JsonLdAdapter.findValue(value).orElseThrow(() -> new JsonLdException(JsonLdErrorCode.INVALID_FRAME));
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
            throw new JsonLdException(JsonLdErrorCode.INVALID_FRAME);
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
        return expanded.keySet();
    }

    public Object get(String property) {
        return expanded.get(property);
    }

    public boolean contains(String property) {
        return expanded.containsKey(property);
    }

    public boolean containsOnly(String property) {
        return expanded.containsKey(property) && ValuePatternMatcher.isWildcard(expanded, property);
    }

    public boolean isWildCard() {
        return ValuePatternMatcher.isWildcard(expanded);
    }

    public boolean isWildCard(String property) {
        return expanded.containsKey(property)
                && ValuePatternMatcher.isWildcard(expanded.get(property));
    }

    public boolean isNone(String property) {
        return expanded.containsKey(property)
                && ValuePatternMatcher.isNone(expanded.get(property));
    }

    public Collection<?> asCollection(String property) {
        final var value = expanded.get(property);

        return value instanceof Collection col
                ? col
                : value != null
                        ? List.of(value)
                        : List.of();
    }

    @Override
    public String toString() {
        return expanded.toString();
    }

    public boolean isValuePattern() {
        return JsonLdAdapter.isValueNode(expanded);
    }

    public boolean matchValue(Object value) {
        return value instanceof Map map && ValuePatternMatcher.match(expanded, map);
    }

    public boolean isDefaultObject(String property) {
        return JsonLdAdapter.isDefault(expanded.get(property))
                || expanded.get(property) instanceof Collection array
                        && array.size() == 1
                        && JsonLdAdapter.isDefault(array.iterator().next());
    }

    public boolean isPattern() {
        return JsonLdAdapter.isNode(expanded);
    }

    public boolean isReference() {
        return JsonLdAdapter.isReference(expanded);
    }

    public boolean matchNode(FramingState state, Object value, boolean requireAll) throws JsonLdException {

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
        return JsonLdAdapter.isList(expanded);
    }

    public boolean isDefault(String graphKey) {
//        if (context != null) {
        // FIXME frameObject
        for (final String key : expanded.keySet()) {
            if (key.equals(graphKey)) {
                return true;
            }
        }
//        }
        return false;
    }
}
