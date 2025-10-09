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
package com.apicatalog.jsonld.expansion;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonMapBuilder;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.node.DefaultObject;
import com.apicatalog.jsonld.node.GraphNode;
import com.apicatalog.jsonld.node.ListNode;
import com.apicatalog.jsonld.node.ValueNode;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.tree.io.JakartaAdapter;
import com.apicatalog.tree.io.NativeMaterializer;
import com.apicatalog.tree.io.PolyMorph;

import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
final class ObjectExpansion1314 {

    private static final Logger LOGGER = Logger.getLogger(ObjectExpansion1314.class.getName());

    // mandatory
    private Context activeContext;

    private final JsonObject element;
    private final String activeProperty;
    private final URI baseUrl;

    private Context typeContext;
    private Map<String, Object> result;
    private String inputType;
    private Map<String, JsonValue> nest;

    // optional
    private boolean frameExpansion;
    private boolean ordered;

    private ObjectExpansion1314(final Context activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
    }

    public static final ObjectExpansion1314 with(final Context activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        return new ObjectExpansion1314(activeContext, element, activeProperty, baseUrl);
    }

    public void expand() throws JsonLdError, IOException {

        // 13.
        for (final String key : Utils.index(element.keySet(), ordered)) {

            // 13.1.
            if (Keywords.CONTEXT.equals(key)) {
                continue;
            }

            activeContext.runtime().tick();

            // 13.2.
            final String expandedProperty = activeContext
                    .uriExpansion()
                    .documentRelative(false)
                    .vocab(true)
                    .expand(key);

            // if the term is undefined and
            if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
                switch (activeContext.runtime().getUndefinedTermPolicy()) {
                case Fail:
                    throw new JsonLdError(JsonLdErrorCode.UNDEFINED_TERM,
                            "An undefined term has been found [" + key + "]. Change policy to Ignore or Warn or define the term in a context");
                case Warn:
                    LOGGER.log(Level.WARNING, "An undefined term has been found [{0}]", key);
                case Ignore:
                    continue;
                }
            }

            JsonValue value = element.get(key);

            // 13.4. If expanded property is a keyword:
            if (Keywords.contains(expandedProperty)) {

                Object expandedValue = null;

                // 13.4.1
                if (Keywords.REVERSE.equals(activeProperty)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_MAP);
                }

                // 13.4.2
                if (result.containsKey(expandedProperty)
                        && Keywords.noneMatch(expandedProperty, Keywords.INCLUDED, Keywords.TYPE)) {
                    throw new JsonLdError(JsonLdErrorCode.COLLIDING_KEYWORDS,
                            "Two properties which expand to the same keyword have been detected. A property '" + key + "'"
                                    + " expands to '" + expandedProperty + "'"
                                    + " but the '" + expandedProperty + "' property is already present.");
                }

                // 13.4.3
                if (Keywords.ID.equals(expandedProperty)) {

                    // Extension: JSON-LD-STAR (Experimental)
//                    if (!activeContext.runtime().isRdfStar() && Keywords.ANNOTATION.equals(activeProperty)) {
//                        throw new JsonLdError(JsonLdErrorCode.INVALID_ANNOTATION);
//
//                    } else if (activeContext.runtime().isRdfStar() && JsonUtils.isNonEmptyObject(value)) {
//
//                        expandedValue = Expansion
//                                .with(activeContext, value, null, baseUrl)
//                                .frameExpansion(frameExpansion)
//                                .ordered(ordered)
//                                .compute();
//
//                        if (!NodeObject.isEmbeddedNode(expandedValue)) {
//                            throw new JsonLdError(JsonLdErrorCode.INVALID_EMBEDDED_NODE);
//                        }
//
//                        // 13.4.3.1
//                    } else 

                    if (!frameExpansion && JsonUtils.isNotString(value) && (!activeContext.runtime().isNumericId() || JsonUtils.isNotNumber(value))
                            || frameExpansion
                                    && JsonUtils.isNotString(value)
                                    && JsonUtils.isNonEmptyObject(value)
                                    && (JsonUtils.isNotArray(value)
                                            || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE, "An @id entry was encountered whose value [" + value + "] was not a string.");

                        // 13.4.3.2
                    } else if (value instanceof JsonString jsonString) {

//                        final String expandedStringValue = activeContext
                        expandedValue = activeContext
                                .uriExpansion()
                                .documentRelative(true)
                                .vocab(false)
                                .expand(jsonString.getString());

                        if (frameExpansion && expandedValue != null) {
                            expandedValue = Set.of(expandedValue);

                        } else if (expandedValue == null) {
                            result.put(Keywords.ID, null);
                            continue;
                        }

//                        if (expandedStringValue != null) {
//
//                            expandedValue = JsonProvider.instance().createValue(expandedStringValue);
//
//                            if (frameExpansion) {
//                                expandedValue = JsonProvider.instance().createArrayBuilder().add(expandedValue).build();
//                            }
//                        } else {
//                            expandedValue = JsonValue.NULL;
//                        }

                    } else if (value instanceof JsonNumber jsonNumber) {

//                        final String expandedStringValue = activeContext
                        expandedValue = activeContext
                                .uriExpansion()
                                .documentRelative(true)
                                .vocab(false)
                                .expand(jsonNumber.toString());

                        if (frameExpansion && expandedValue != null) {
                            expandedValue = Set.of(expandedValue);

                        } else if (expandedValue == null) {
                            result.put(Keywords.ID, null);
                            continue;
                        }

//                        if (expandedStringValue != null) {
//
//                            expandedValue = JsonProvider.instance().createValue(expandedStringValue);
//
//                            if (frameExpansion) {
//                                expandedValue = JsonProvider.instance().createArrayBuilder().add(expandedValue).build();
//                            }
//                        } else {
//                            expandedValue = JsonValue.NULL;
//                        }

                    } else if (JsonUtils.isObject(value)) {

                        expandedValue = Set.of(Collections.emptyMap());
//                        expandedValue = JsonProvider.instance().createArrayBuilder().add(JsonValue.EMPTY_JSON_OBJECT).build();

                    } else if (JsonUtils.isEmptyArray(value)) {

                        expandedValue = Collections.emptySet();
//                        expandedValue = JsonValue.EMPTY_JSON_ARRAY;

                    } else if (JsonUtils.isArray(value)) {

//                        final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();

                        var array = new HashSet<>(value.asJsonArray().size());

                        for (final JsonValue item : JsonUtils.toCollection(value)) {

                            String expandedStringValue = activeContext
                                    .uriExpansion()
                                    .documentRelative(true)
                                    .vocab(false)
                                    .expand(((JsonString) item).getString());

                            if (expandedStringValue != null) {
                                array.add(expandedStringValue);
                            }
                        }

                        expandedValue = array;
                    }

                }

                // 13.4.4
                else if (Keywords.TYPE.equals(expandedProperty)) {
                    // 13.4.4.1
                    if ((!frameExpansion
                            && JsonUtils.isNotString(value)
                            && (JsonUtils.isNotArray(value)
                                    || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString)))
                            || frameExpansion
                                    && JsonUtils.isNotString(value)
                                    && JsonUtils.isNonEmptyObject(value)
                                    && (JsonUtils.isNotArray(value)
                                            || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString))
                                    && !DefaultObject.isDefaultObject(value)
                                    && DefaultObject.getValue(value)
                                            .filter(JsonUtils::isString)
                                            .map(JsonString.class::cast)
                                            .map(JsonString::getString)
                                            .map(UriUtils::isNotURI)
                                            .orElse(true)) {

                        throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + value + "].");
                    }

                    // 13.4.4.2
                    if (JsonUtils.isEmptyObject(value)) {
                        expandedValue = Collections.emptyMap();

                        // 13.4.4.3
                    } else if (DefaultObject.isDefaultObject(value)) {

                        final Optional<JsonValue> defaultValue = DefaultObject.getValue(value);

                        if (defaultValue.filter(JsonUtils::isString).isPresent()) {

                            expandedValue = Map.of(
                                    Keywords.DEFAULT,
                                    typeContext
                                            .uriExpansion()
                                            .vocab(true)
                                            .documentRelative(true)
                                            // deepcode ignore checkIsPresent~Optional: false positive
                                            .expand(defaultValue
                                                    .map(JsonString.class::cast)
                                                    .map(JsonString::getString)
                                                    .get()));
                        }

                        // 13.4.4.4
                    } else {

                        if (value instanceof JsonString jsonString) {

                            expandedValue = typeContext
                                    .uriExpansion()
                                    .vocab(true)
                                    .documentRelative(true)
                                    .expand(jsonString.getString());

                            if (expandedValue == null) {
                                result.put(Keywords.VALUE, null);
                                continue;
                            }

                        } else if (JsonUtils.isArray(value)) {

//                            final JsonArrayBuilder array = JsonProvider.instance().createArrayBuilder();

                            var array = new HashSet<>(value.asJsonArray().size());

                            for (final JsonValue item : value.asJsonArray()) {

                                if (item instanceof JsonString jsonString) {

                                    final String expandedStringValue = typeContext
                                            .uriExpansion()
                                            .vocab(true)
                                            .documentRelative(true)
                                            .expand(jsonString.getString());

                                    if (expandedStringValue != null) {
                                        array.add(expandedStringValue);
//                                        array.add(JsonProvider.instance().createValue(expandedStringValue));
                                    }
                                }
                            }
                            expandedValue = array;
                        }
                    }

                    // 13.4.4.5
                    if (result.containsKey(Keywords.TYPE)) {

                        final Object typeValue = result.get(Keywords.TYPE);

//                        final JsonValue typeValue = result.get(Keywords.TYPE).orElse(null);

                        if (typeValue instanceof Collection<?> set) {

                            var newSet = (new LinkedHashSet<Object>(set));
                            newSet.add(expandedValue);
                            expandedValue = newSet;

                        } else {
                            expandedValue = Set.of(typeValue, expandedValue);
                        }
//                        if (JsonUtils.isArray(typeValue)) {
//                            expandedValue = JsonProvider.instance().createArrayBuilder(typeValue.asJsonArray()).add(expandedValue).build();
//
//                        } else {
//                            expandedValue = JsonProvider.instance().createArrayBuilder().add(typeValue).add(expandedValue).build();
//                        }
                    }
                }

                // 13.4.5
                else if (Keywords.GRAPH.equals(expandedProperty)) {
                    expandedValue = JsonUtils.toList(Expansion
//                    expandedValue = JsonUtils.toJsonArray(Expansion
                            .with(typeContext, value, Keywords.GRAPH, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute());
                }

                // 13.4.6
                else if (Keywords.INCLUDED.equals(expandedProperty)) {

                    // 13.4.6.1
                    if (activeContext.runtime().isV10()) {
                        continue;
                    }

                    // 13.4.6.2
                    expandedValue = Expansion
                            .with(activeContext, value, null, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute();

                    if (expandedValue != null) {

                        if (!(expandedValue instanceof Collection)) {
                            expandedValue = Set.of(expandedValue);
                        }

                        // FIXME
//                        if (((Collection<?>) expandedValue).stream().anyMatch(NodeObject::isNotNodeObject)) {
//                            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
//                        }

//                        if (JsonUtils.isNotArray(expandedValue)) {
//                            expandedValue = JsonProvider.instance().createArrayBuilder().add(expandedValue).build();
//                        }
//
//                        // 13.4.6.3
//                        if (expandedValue.asJsonArray().stream().anyMatch(NodeObject::isNotNodeObject)) {
//                            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
//                        }

                        // 13.4.6.4
                        if (result.containsKey(Keywords.INCLUDED)) {

//                            final JsonValue includedValue = result.get(Keywords.INCLUDED).orElse(null);
                            final Object includedValue = result.get(Keywords.INCLUDED);

                            final Set<Object> included;

                            if (includedValue instanceof Collection<?> set) {
                                included = new LinkedHashSet<>(set);
                                included.addAll((Collection<?>) expandedValue);

                            } else {
                                included = new LinkedHashSet<>((Collection<?>) expandedValue);
                                included.add(includedValue);
                            }

                            expandedValue = Set.copyOf(included);

//                            if (JsonUtils.isArray(includedValue)) {
//                                included = JsonProvider.instance().createArrayBuilder(includedValue.asJsonArray());
//
//                            } else {
//                                included = JsonProvider.instance().createArrayBuilder().add(includedValue);
//                            }

//                            expandedValue.asJsonArray().forEach(included::add);

//                            expandedValue = included.build();
                        }
                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                    }
                }

                // 13.4.7
                if (Keywords.VALUE.equals(expandedProperty)) {

                    // 13.4.7.1
                    if (Keywords.JSON.equals(inputType)) {

                        if (activeContext.runtime().isV10()) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
                        }

                        expandedValue = new PolyMorph(value, JakartaAdapter.instance());
                                
                        // 13.4.7.2
                    } else if (JsonUtils.isNull(value)
                            || JsonUtils.isScalar(value)
                            || frameExpansion
                                    && (JsonUtils.isEmptyObject(value)
                                            || JsonUtils.isEmptyArray(value)
                                            || JsonUtils.isArray(value)
                                                    && value.asJsonArray().stream().allMatch(JsonUtils::isScalar))) {

                        expandedValue = new NativeMaterializer().node(value, JakartaAdapter.instance());

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toList(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
                    }

                    // 13.4.7.4
                    if (expandedValue == null) {
                        result.put(Keywords.VALUE, null);
                        continue;
                    }
                }

                // 13.4.8
                if (Keywords.LANGUAGE.equals(expandedProperty)) {

                    // 13.4.8.1
                    if (JsonUtils.isString(value)
                            || frameExpansion
                                    && (JsonUtils.isEmptyObject(value)
                                            || JsonUtils.isEmptyArray(value)
                                            || JsonUtils.isArray(value)
                                                    && value.asJsonArray().stream().allMatch(JsonUtils::isString))) {

                        if (value instanceof JsonString jsonString) {

                            if (!LanguageTag.isWellFormed(jsonString.getString())) {
                                LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", jsonString.getString());
                            }

                            // 13.4.8.2
                            expandedValue = jsonString.getString().toLowerCase();

                        } else {
                            expandedValue = JsonUtils.asScalar(value);
                        }

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toList(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                    }
                }

                // 13.4.9.
                if (Keywords.DIRECTION.equals(expandedProperty)) {
                    // 13.4.9.1.
                    if (activeContext.runtime().isV10()) {
                        continue;
                    }

                    // 13.4.9.2.
                    if ((JsonUtils.isString(value)
                            && "ltr".equals(((JsonString) value).getString()) || "rtl".equals(((JsonString) value).getString()))
                            || frameExpansion
                                    && (JsonUtils.isEmptyObject(value)
                                            || JsonUtils.isEmptyArray(value)
                                            || JsonUtils.isArray(value)
                                                    && value.asJsonArray().stream().allMatch(JsonUtils::isString))) {

                        // 13.4.9.3.
                        expandedValue = value;

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toList(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    }
                }

                // 13.4.10.
                if (Keywords.INDEX.equals(expandedProperty)) {

                    // 13.4.10.1.
                    if (value instanceof JsonString jsonString) {
                        // 13.4.10.2
                        expandedValue = jsonString.getString();

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INDEX_VALUE);
                    }
                }

                // 13.4.11
                if (Keywords.LIST.equals(expandedProperty)) {

                    // 13.4.11.1
                    if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {
                        continue;
                    }

                    // 13.4.11.1
                    expandedValue = Expansion
                            .with(activeContext, value, activeProperty, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute();

                    if (!(expandedValue instanceof Collection<?>)) {
                        expandedValue = Set.of(expandedValue);
                    }

//                    if (JsonUtils.isNotArray(expandedValue)) {
//                        expandedValue = JsonProvider.instance().createArrayBuilder().add(expandedValue).build();
//                    }
                }

                // 13.4.12
                if (Keywords.SET.equals(expandedProperty)) {
                    expandedValue = Expansion
                            .with(activeContext, value, activeProperty, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute();
                }

                // 13.4.13
                if (Keywords.REVERSE.equals(expandedProperty)) {

                    // 13.4.13.1.
                    if (JsonUtils.isNotObject(value)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_REVERSE_VALUE);
                    }

                    // 13.4.13.2.
                    expandedValue = Expansion
                            .with(activeContext, value, Keywords.REVERSE, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute();

                    if (expandedValue instanceof Map expandedValueObject) {
//                    if (JsonUtils.isObject(expandedValue)) {

//                        final JsonObject expandedValueObject = expandedValue.asJsonObject();
//                        final JsonObject expandedValueObject = expandedValue.asJsonObject();
                        // FIXME
                        // 13.4.13.3.
                        if (expandedValueObject.containsKey(Keywords.REVERSE)) {

                            for (final Entry<String, Object> entry : ((Map<String, Object>) expandedValueObject.get(Keywords.REVERSE)).entrySet()) {
                                // 13.4.13.3.1.
                                result.put(entry.getKey(), entry.getValue());
                            }
                        }

                        // 13.4.13.4.
                        if (expandedValueObject.size() > 1
                                || !expandedValueObject.containsKey(Keywords.REVERSE)) {

                            var reverseMap = new HashMap<String, Object>();


//                            final JsonMapBuilder reverseMap = result.getMapBuilder(Keywords.REVERSE);
//
//                            // 13.4.13.4.2
                            for (var entry : ((Map<String, Object>) expandedValueObject).entrySet()) {

                                if (Keywords.REVERSE.equals(entry.getKey())) {
                                    continue;
                                }

                                // 13.4.13.4.2.1
                                if (entry.getValue() instanceof Collection<?> collection) {

                                    for (var item : collection) {

                                        // 13.4.13.4.2.1.1
                                        if (ListNode.isListNode(item) || ValueNode.isValueNode(item)) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                                        }

                                        // 13.4.13.4.2.1.1
                                        JsonMapBuilder.merge(reverseMap, entry.getKey(), item);
                                    }
                                }
                            }

                            if (!reverseMap.isEmpty()) {
                                result.put(Keywords.REVERSE, reverseMap);

//                                result.remove(Keywords.REVERSE);
                            }
                        }
                    }

                    // 13.4.13.5.
                    continue;
                }

                // 13.4.14
                if (Keywords.NEST.equals(expandedProperty)) {

                    if (nest == null) {
                        nest = new LinkedHashMap<>();
                        nest.put(key, JsonValue.EMPTY_JSON_ARRAY);

                    } else if (!nest.containsKey(key)) {
                        nest.put(key, JsonValue.EMPTY_JSON_ARRAY);
                    }

                    continue;
                }

                // Extension: JSON-LD-STAR (Experimental)
                if (Keywords.ANNOTATION.equals(expandedProperty)) {

                    if (!activeContext.runtime().isRdfStar()) {
                        continue;
                    }

                    expandedValue = JsonUtils.toList(Expansion
                            .with(activeContext, value, Keywords.ANNOTATION, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute());
                }

                // 13.4.15
                if (frameExpansion
                        && (Keywords.DEFAULT.equals(expandedProperty)
                                || Keywords.EMBED.equals(expandedProperty)
                                || Keywords.EXPLICIT.equals(expandedProperty)
                                || Keywords.OMIT_DEFAULT.equals(expandedProperty)
                                || Keywords.REQUIRE_ALL.equals(expandedProperty))) {

                    expandedValue = Expansion.with(activeContext, value, expandedProperty, baseUrl)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .compute();
                }

                // 13.4.16
                if (expandedValue != null
                        || (Keywords.VALUE.equals(expandedProperty)
                                && Keywords.JSON.equals(inputType))) {

//                    JsonMapBuilder.merge(result, expandedProperty, expandedValue);
                    result.put(expandedProperty, expandedValue);
                }

                // 13.4.17
                continue;
            }

            // 13.5.
            final Optional<TermDefinition> keyTermDefinition = activeContext.getTerm(key);

            final Collection<String> containerMapping = keyTermDefinition
                    .map(TermDefinition::getContainerMapping)
                    .orElse(Collections.emptyList());

//            JsonValue expandedValue = null;
            Object expandedValue = null;

            // 13.6.
            if (keyTermDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.JSON::equals)
                    .isPresent()) {

//                (JsonStructure) new JakartaMaterializer().node(collection, new JsonLdAdapter())

//                expandedValue = new NativeMaterializer().node(value, JakartaAdapter.instance());
                expandedValue = new PolyMorph(value, JakartaAdapter.instance());

//                if (expandedValue != null) {
                    expandedValue = Map.of(
                            Keywords.TYPE, Keywords.JSON,
                            Keywords.VALUE, expandedValue);

//                } else {
//                    var map = new HashMap<>(2);
//                    map.put(Keywords.TYPE, Keywords.JSON);
//                    map.put(Keywords.VALUE, null);
//                    expandedValue = map;
//                }

//                expandedValue = JsonProvider.instance().createObjectBuilder().add(Keywords.VALUE, value)
//                        .add(Keywords.TYPE, Keywords.JSON).build();

                // 13.7.
            } else if (containerMapping.contains(Keywords.LANGUAGE) && JsonUtils.isObject(value)) {

                // 13.7.1.
//                expandedValue = JsonValue.EMPTY_JSON_ARRAY;
                Set<Object> langMaps = new LinkedHashSet<>();

                // 13.7.2.
                final DirectionType direction = keyTermDefinition
                        .map(TermDefinition::getDirectionMapping)
                        .orElseGet(() -> activeContext.getDefaultBaseDirection());

                final JsonObject valueObject = value.asJsonObject();

                // 13.7.4.
                for (final String langCode : Utils.index(valueObject.keySet(), ordered)) {

                    JsonValue langValue = valueObject.get(langCode);

                    // 13.7.4.1.
                    if (JsonUtils.isNotArray(langValue)) {
                        langValue = JsonProvider.instance().createArrayBuilder().add(langValue).build();
//                        langValue =langValue).build();
                    }

                    // 13.7.4.2.
                    for (final JsonValue item : langValue.asJsonArray()) {

                        // 13.7.4.2.1.
                        if (JsonUtils.isNull(item)) {
                            continue;
                        }

                        // 13.7.4.2.2.
                        if (JsonUtils.isNotString(item)) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAP_VALUE);
                        }

                        // 13.7.4.2.3.
//                        final JsonObjectBuilder langMap = JsonProvider.instance()
//                                .createObjectBuilder()
//                                .add(Keywords.VALUE, item);

                        final Map<String, Object> langMap = new HashMap<>();
                        langMap.put(Keywords.VALUE, ((JsonString) item).getString());

                        // 13.7.4.2.4.
                        if (!Keywords.NONE.equals(langCode)) {

                            final String expandedLangCode = activeContext
                                    .uriExpansion()
                                    .vocab(true)
                                    .expand(langCode);

                            if (!Keywords.NONE.equals(expandedLangCode)) {

                                if (!LanguageTag.isWellFormed(langCode)) {
                                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", langCode);
                                }

                                langMap.put(Keywords.LANGUAGE, langCode.toLowerCase());
                            }
                        }

                        // 13.7.4.2.5.
                        if (direction != null && !DirectionType.NULL.equals(direction)) {
                            langMap.put(Keywords.DIRECTION, direction.name().toLowerCase());
                        }

                        // 13.7.4.2.6.
                        langMaps.add(langMap);
//                        expandedValue = JsonProvider.instance().createArrayBuilder(expandedValue.asJsonArray()).add(langMap).build();
                    }
                }

                expandedValue = Set.copyOf(langMaps);

                // 13.8.
            } else if ((containerMapping.contains(Keywords.INDEX) || containerMapping.contains(Keywords.TYPE)
                    || containerMapping.contains(Keywords.ID)) && JsonUtils.isObject(value)) {

                // 13.8.1.
//                expandedValue = JsonValue.EMPTY_JSON_ARRAY;
                var indices = new ArrayList<>();

                // 13.8.2.
                final String indexKey = keyTermDefinition
                        .map(TermDefinition::getIndexMapping)
                        .orElse(Keywords.INDEX);

                // 13.8.3.
                for (final String index : Utils.index(value.asJsonObject().keySet(), ordered)) {

                    JsonValue indexValue = value.asJsonObject().get(index);

                    // 13.8.3.1.
                    Context mapContext = activeContext;

                    if (activeContext.getPreviousContext() != null
                            && ((containerMapping.contains(Keywords.ID) && !containerMapping.contains(Keywords.SET)) || containerMapping.contains(Keywords.TYPE))) {
                        mapContext = activeContext.getPreviousContext();
                    }

                    // 13.8.3.2.
                    final Optional<TermDefinition> indexTermDefinition = mapContext.getTerm(index);

                    if (containerMapping.contains(Keywords.TYPE)
                            && indexTermDefinition.map(TermDefinition::getLocalContext).isPresent()) {

                        mapContext = mapContext
                                .newContext()
                                .create(
                                        indexTermDefinition.get().getLocalContext(),
                                        indexTermDefinition.get().getBaseUrl());
                    }

                    // 13.8.3.3.
                    if (mapContext == null) {
                        mapContext = activeContext;
                    }

                    // 13.8.3.4.
                    String expandedIndex = activeContext
                            .uriExpansion()
                            .vocab(true)
                            .expand(index);

                    // 13.8.3.5.
                    if (JsonUtils.isNotArray(indexValue)) {
                        indexValue = JsonProvider.instance().createArrayBuilder().add(indexValue).build();
                    }

                    // 13.8.3.6.
                    // FIXME
                    Collection<?> indexValues = ArrayExpansion
                            .with(mapContext, indexValue.asJsonArray(), key, baseUrl)
                            .fromMap(true)
                            .frameExpansion(frameExpansion)
                            .ordered(ordered)
                            .expand();

//                    Collection<Object> indexValues = (Collection)Expansion.with(mapContext, indexValue, key, baseUrl)
//                            .fromMap(true)
//                            .frameExpansion(frameExpansion)
//                            .ordered(ordered).compute();

                    // 13.8.3.7.
                    for (var item : indexValues) {

                        var result = new HashMap<String, Object>();

                        // 13.8.3.7.1.
                        if (containerMapping.contains(Keywords.GRAPH) && !GraphNode.isGraph(item)) {
                            result.put(Keywords.GRAPH, Set.of(item));

                        } else {
                            result.putAll((Map) item);
                        }

                        // 13.8.3.7.2.
                        if (containerMapping.contains(Keywords.INDEX)
                                && !Keywords.INDEX.equals(indexKey)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.2.1.
                            // FIXME
//                            JsonValue reExpandedIndex = activeContext.valueExpansion()
//                                    .expand(JsonProvider.instance().createValue(index), indexKey);

                            var reExpandedIndex = activeContext.expandValue(indexKey, JsonProvider.instance().createValue(index));

                            // 13.8.3.7.2.2.
                            var expandedIndexKey = activeContext
                                    .uriExpansion()
                                    .vocab(true)
                                    .expand(indexKey);

                            // 13.8.3.7.2.3.
//                            final List<?> indexPropertyValues = JsonProvider.instance().createArrayBuilder().add(reExpandedIndex);
                            var indexPropertyValues = new ArrayList<>();
                            indexPropertyValues.add(reExpandedIndex);

//                            final JsonValue existingValues = item.asJsonObject().get(expandedIndexKey);
                            var existingValues = result.get(expandedIndexKey);

                            if (existingValues instanceof Collection<?> values) {
                                indexPropertyValues.addAll(values);
//                                    existingValues
//                                            .stream()
//                                            .map(JsonUtils::asScalar)
//                                            .forEach(indexPropertyValues::add);

                            } else if (existingValues != null) {
                                indexPropertyValues.add(existingValues);
                            }

                            // 13.8.3.7.2.4.
//                            item = JsonProvider.instance().createObjectBuilder(item.asJsonObject())
//                                    .add(expandedIndexKey, indexPropertyValues).build();

                            result.put(expandedIndexKey, indexPropertyValues);

                            // 13.8.3.7.2.5.
                            if (ValueNode.isValueNode(item) && result.size() > 1) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
                            }

                            // 13.8.3.7.3.
                        } else if (containerMapping.contains(Keywords.INDEX)
                                && !result.containsKey(Keywords.INDEX)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            result.put(Keywords.INDEX, index);

                            // 13.8.3.7.4.
                        } else if (containerMapping.contains(Keywords.ID)
                                && !result.containsKey(Keywords.ID)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            result.put(Keywords.ID, activeContext
                                    .uriExpansion()
                                    .vocab(false)
                                    .documentRelative(true)
                                    .expand(index));

                            // 13.8.3.7.5.
                        } else if (containerMapping.contains(Keywords.TYPE)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            final List<Object> types = new ArrayList<>();
                            types.add(expandedIndex);

                            final Object existingType = result.get(Keywords.TYPE);

                            if (existingType != null) {
                                if (existingType instanceof Collection<?> existingTypes) {
                                    types.addAll(existingTypes);

                                } else {
                                    types.add(existingType);
                                }
                            }

                            result.put(Keywords.TYPE, types);
                        }
                        // 13.8.3.7.6.
                        indices.add(result);
                    }
                }
                expandedValue = indices;

                // 13.9.
            } else {
                expandedValue = Expansion
                        .with(activeContext, value, key, baseUrl)
                        .frameExpansion(frameExpansion)
                        .ordered(ordered)
                        .compute();
            }

            // 13.10.
            if (expandedValue == null) {
                continue;
            }

            // 13.11.
            if (containerMapping.contains(Keywords.LIST) && !ListNode.isListNode(expandedValue)) {
                expandedValue = ListNode.asListNode(expandedValue);

            }

            // 13.12.
            if (containerMapping.contains(Keywords.GRAPH)
                    && !containerMapping.contains(Keywords.ID)
                    && !containerMapping.contains(Keywords.INDEX)) {

                if (expandedValue instanceof Collection<?> expandedValues) {

                    var list = new ArrayList<Object>(expandedValues.size());

                    for (var item : expandedValues) {
                        list.add(Map.of(Keywords.GRAPH, Set.of(item)));
                    }

                    expandedValue = list;

                } else {
                    expandedValue = Set.of(Map.of(Keywords.GRAPH, Set.of(expandedValue)));
                }
            }

            // 13.13.
            if (keyTermDefinition.filter(TermDefinition::isReverseProperty).isPresent()) {

                expandedValue = JsonUtils.asCollection(expandedValue);

                // 13.13.4.
                for (Object item : (Collection<?>) expandedValue) {

                    // 13.13.4.1.
                    if (ListNode.isListNode(item) || ValueNode.isValueNode(item)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                    }

                    // 13.13.4.3.
                    Map<String, Object> map = (Map<String, Object>) result.get(Keywords.REVERSE);
                    if (map == null) {
                        result.put(Keywords.REVERSE, Map.of(expandedProperty, Set.of(item)));

                    } else if (map instanceof HashMap) {
                        JsonMapBuilder.merge(map, expandedProperty, item);

                    } else {
                        map = new HashMap<>(map);
                        JsonMapBuilder.merge(map, expandedProperty, item);
                        result.put(Keywords.REVERSE, map);
                    }
                }

                // 13.14
            } else {

                JsonMapBuilder.merge(result, expandedProperty, expandedValue);
//                result.add(expandedProperty, expandedValue);
            }
        }

        // 14.
        if (nest != null) {
            processNest();
        }
    }

    public ObjectExpansion1314 frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public ObjectExpansion1314 ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion1314 nest(Map<String, JsonValue> nest) {
        this.nest = nest;
        return this;
    }

    public ObjectExpansion1314 typeContext(Context typeContext) {
        this.typeContext = typeContext;
        return this;
    }

    public ObjectExpansion1314 result(Map<String, Object> result) {
        this.result = result;
        return this;
    }

    public ObjectExpansion1314 inputType(String inputType) {
        this.inputType = inputType;
        return this;
    }

    private void recurse() throws JsonLdError, IOException {

        activeContext.runtime().tick();

        // step 3
        final Optional<JsonValue> propertyContext = activeContext
                .getTerm(activeProperty)
                .map(TermDefinition::getLocalContext);

        // step 8
        if (propertyContext.isPresent()) {
            activeContext = activeContext
                    .newContext()
                    .overrideProtected(true)
                    .create(
                            propertyContext.get(),
                            activeContext
                                    .getTerm(activeProperty)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null));
        }

        // steps 13-14
        expand();
    }

    private final void processNest() throws JsonLdError, IOException {

        for (final String nestedKey : Utils.index(nest.keySet(), ordered)) {

            // 14.2.
            for (final JsonValue nestValue : JsonUtils.toCollection(element.get(nestedKey))) {

                // 14.2.1
                if (JsonUtils.isNotObject(nestValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                }

                for (final String nestedValueKey : nestValue.asJsonObject().keySet()) {

                    if (Keywords.VALUE.equals(typeContext
                            .uriExpansion()
                            .vocab(true)
                            .expand(nestedValueKey))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                }

                // 14.2.2
                ObjectExpansion1314
                        .with(activeContext, nestValue.asJsonObject(), nestedKey, baseUrl)
                        .inputType(inputType)
                        .result(result)
                        .typeContext(typeContext)
                        .nest(new LinkedHashMap<>())
                        .frameExpansion(frameExpansion)
                        .ordered(ordered)
                        .recurse();
            }
        }
    }
}
