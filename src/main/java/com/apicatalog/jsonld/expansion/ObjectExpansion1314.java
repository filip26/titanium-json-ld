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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.Direction;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.uri.UriUtils;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
final class ObjectExpansion1314 {

    private static final Logger LOGGER = Logger.getLogger(ObjectExpansion1314.class.getName());

    private Context typeContext;
    private Map<String, Object> result;
    private String inputType;
    private Map<String, Object> nest;

    private final Params params;

    private ObjectExpansion1314(Params params) {
        this.params = params;
    }

    public static ObjectExpansion1314 with(Params params) {
        return new ObjectExpansion1314(params);
    }

    public void expand(
            final Context activeContext,
            final Map<String, ?> element,
            final String activeProperty,
            final String term) throws JsonLdException {

        final var keys = params.options().isOrdered()
                ? element.keySet().stream().sorted().iterator()
                : element.keySet().iterator();

        // 13.
        while (keys.hasNext()) {

            var key = keys.next();

            // 13.1.
            if (Keywords.CONTEXT.equals(key)) {
                continue;
            }

            // 13.2.
            var expandedProperty = UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                    .documentRelative(false)
                    .vocab(true)
                    .expand(key);

            // if the term is undefined, i.e. is not URI or keyword
            if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
                params.runtime().fire(EventType.UNDEFINED_TERM, key);
                continue;
            }

            // 13.4. If expanded property is a keyword:
            if (Keywords.contains(expandedProperty)) {

                if (!Keywords.contains(key) || !Keywords.contains(expandedProperty)) {
                    params.runtime().fire(EventType.TERM_KEY, key, expandedProperty);
                }

                final var value = element.get(key);

                keyword(activeContext, value, activeProperty, key, expandedProperty);

                // 13.4.17
                continue;
            }

            // then it's not a keyword

            // 13.5.
            final var keyTermDefinition = activeContext.findTerm(key);

            final var typeMapping = keyTermDefinition.map(TermDefinition::getTypeMapping).orElse(null);

            if (typeMapping != null) {
                params.runtime().fire(EventType.TYPE_KEY, key, typeMapping);
            }

            final var containerMapping = keyTermDefinition
                    .map(TermDefinition::getContainerMapping)
                    .orElse(List.of());

            final var value = element.get(key);
            Object expandedValue = null;

            // 13.6.
            if (keyTermDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.JSON::equals)
                    .isPresent()) {

                expandedValue = Map.of(
                        Keywords.TYPE,
                        Keywords.JSON,
                        Keywords.VALUE,
                        value);
            }
            // 13.7.
            else if (value instanceof Map map && containerMapping.contains(Keywords.LANGUAGE)) {

                // 13.7.1.
                final var langMaps = new ArrayList<>();

                // 13.7.2.
                final var direction = keyTermDefinition
                        .map(TermDefinition::getDirectionMapping)
                        .orElseGet(() -> activeContext.getDefaultBaseDirection());

                // 13.7.4.
                final var langCodes = params.options().isOrdered()
                        ? map.keySet().stream().sorted().iterator()
                        : map.keySet().iterator();

                params.runtime().fire(EventType.BEGIN_MAP, key);
                while (langCodes.hasNext()) {

                    final var langCode = (String) langCodes.next();
                    final var langValue = map.get(langCode);

                    // 13.7.4.2.
                    for (final var item : JsonLdAdapter.asCollection(langValue)) {

                        // 13.7.4.2.1.
                        if (item == null) {
                            continue;
                        }

                        // 13.7.4.2.2.
                        if (!(item instanceof String stringValue)) {
                            throw new JsonLdException(ErrorCode.INVALID_LANGUAGE_MAP_VALUE);
                        }

                        final var langMap = new LinkedHashMap<>();
                        langMap.put(Keywords.VALUE, stringValue);

                        // 13.7.4.2.4.
                        if (!Keywords.NONE.equals(langCode)) {

                            final var expandedLangCode = UriExpansion
                                    .with(activeContext, params.options().loader(), params.runtime())
                                    .vocab(true)
                                    .expand((String) langCode);

                            if (!Keywords.NONE.equals(expandedLangCode)) {

                                if (!LanguageTag.isWellFormed((String) langCode)) {
                                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.",
                                            (String) langCode);
                                }

                                langMap.put(Keywords.LANGUAGE, ((String) langCode).toLowerCase());
                            }
                        }

                        // 13.7.4.2.5.
                        if (direction != null && direction != Direction.NULL) {
                            langMap.put(Keywords.DIRECTION, direction.name().toLowerCase());
                        }

                        // 13.7.4.2.6.
                        langMaps.add(langMap);
                    }
                    params.runtime().fire(EventType.TERM_KEY, langCode, Keywords.LANGUAGE);
                }
                params.runtime().fire(EventType.END_MAP, key);

                expandedValue = List.copyOf(langMaps);

            } else if (value instanceof Map<?, ?> map
                    && (containerMapping.contains(Keywords.INDEX)
                            || containerMapping.contains(Keywords.TYPE)
                            || containerMapping.contains(Keywords.ID))) {

                // 13.8.1.
                final var indices = new ArrayList<>();

                // 13.8.2.
                final var indexKey = keyTermDefinition
                        .map(TermDefinition::getIndexMapping)
                        .orElse(Keywords.INDEX);

                final var valueKeys = params.options().isOrdered()
                        ? map.keySet().stream().sorted().iterator()
                        : map.keySet().iterator();

                params.runtime().fire(EventType.BEGIN_MAP, key);

                // 13.8.3.
                while (valueKeys.hasNext()) {

                    final var valueKey = valueKeys.next();

                    final var indexValue = map.get(valueKey);

                    final var index = (String) valueKey;

                    params.runtime().fire(EventType.TERM_KEY, index, Keywords.INDEX);

                    // 13.8.3.1.
                    Context mapContext = activeContext;

                    if (activeContext.getPreviousContext() != null
                            && ((containerMapping.contains(Keywords.ID) && !containerMapping.contains(Keywords.SET))
                                    || containerMapping.contains(Keywords.TYPE))) {
                        mapContext = activeContext.getPreviousContext();
                    }

                    // 13.8.3.2.
                    final var indexTermDefinition = mapContext.findTerm(index).orElse(null);

                    if (containerMapping.contains(Keywords.TYPE)
                            && indexTermDefinition != null
                            && indexTermDefinition.getLocalContext() != null) {

                        // TODO contextKeys?

                        mapContext = mapContext
                                .newContext(params.options().loader(), params.runtime())
                                .build(
                                        indexTermDefinition.getLocalContext(),
                                        indexTermDefinition.getBaseUrl());
                    }

                    // 13.8.3.3.
                    if (mapContext == null) {
                        mapContext = activeContext;
                    }

                    // 13.8.3.4.
                    final var expandedIndex = UriExpansion
                            .with(activeContext, params.options().loader(), params.runtime())
                            .vocab(true)
                            .expand(index);

                    // 13.8.3.6.
                    final var indexValues = Expansion.array(
                            mapContext,
                            (Collection<?>) indexValue,
                            key,
                            index,
                            Keywords.INDEX,
                            new Params(
                                    params.frameExpansion(),
                                    true,
                                    params.baseUrl(),
                                    params.options(),
                                    params.runtime()));

                    // 13.8.3.7.
                    for (final var item : indexValues) {

                        final var indexMap = new LinkedHashMap<String, Object>();

                        // 13.8.3.7.1.
                        if (containerMapping.contains(Keywords.GRAPH) && JsonLdAdapter.isNotGraph(item)) {
                            indexMap.put(Keywords.GRAPH, List.of(item));

                        } else {
                            @SuppressWarnings("unchecked")
                            final var typedItems = (Map<String, ?>) item;

                            indexMap.putAll(typedItems);
                        }

                        // 13.8.3.7.2.
                        if (containerMapping.contains(Keywords.INDEX)
                                && !Keywords.INDEX.equals(indexKey)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.2.1.
                            final var reExpandedIndex = ValueExpansion.expand(
                                    activeContext,
                                    indexKey,
                                    index,
                                    params);

                            // 13.8.3.7.2.2.
                            final var expandedIndexKey = UriExpansion
                                    .with(activeContext, params.options().loader(), params.runtime())
                                    .vocab(true)
                                    .expand(indexKey);

                            // 13.8.3.7.2.3.
                            final var indexPropertyValues = new ArrayList<>();
                            indexPropertyValues.add(reExpandedIndex);

                            final var existingValues = indexMap.get(expandedIndexKey);

                            if (existingValues instanceof Collection<?> values) {
                                indexPropertyValues.addAll(values);

                            } else if (existingValues != null) {
                                indexPropertyValues.add(existingValues);
                            }

                            // 13.8.3.7.2.4.
                            indexMap.put(expandedIndexKey, indexPropertyValues);

                            // 13.8.3.7.2.5.
                            if (item instanceof Map itemMap
                                    && JsonLdAdapter.isValueNode(itemMap)
                                    && indexMap.size() > 1) {
//TODO?                                    && indexMap.size() > 1) {
                                throw new JsonLdException(ErrorCode.INVALID_VALUE_OBJECT);
                            }

                        } else if (containerMapping.contains(Keywords.INDEX)
                                && !indexMap.containsKey(Keywords.INDEX)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.4.
                            indexMap.put(Keywords.INDEX, index);

                        } else if (containerMapping.contains(Keywords.ID)
                                && !indexMap.containsKey(Keywords.ID)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.4.
                            indexMap.put(
                                    Keywords.ID,
                                    UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                                            .vocab(false)
                                            .documentRelative(true)
                                            .expand(index));

                        } else if (containerMapping.contains(Keywords.TYPE)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.5.
                            var types = new ArrayList<>();
                            types.add(expandedIndex);

                            final Object existingType = indexMap.get(Keywords.TYPE);

                            if (existingType != null) {
                                if (existingType instanceof Collection<?> existingTypes) {
                                    types.addAll(existingTypes);

                                } else {
                                    types.add(existingType);
                                }
                            }

                            indexMap.put(Keywords.TYPE, types);
                        }
                        // 13.8.3.7.6.
                        indices.add(indexMap);
                    }
                }
                params.runtime().fire(EventType.END_MAP, key);
                expandedValue = indices;
            }
            // 13.9.
            else {
                expandedValue = Expansion.expand(
                        activeContext,
                        value,
                        key,
                        key,
                        expandedProperty,
                        params);
            }

            // 13.10.
            if (expandedValue == null) {
                continue;
            }

            // 13.11.
            if (containerMapping.contains(Keywords.LIST)
                    && (!(expandedValue instanceof Map expandedMap)
                            || !JsonLdAdapter.isList(expandedMap))) {
                expandedValue = JsonLdAdapter.toList(expandedValue);
            }

            // 13.12.
            if (containerMapping.contains(Keywords.GRAPH)
                    && !containerMapping.contains(Keywords.ID)
                    && !containerMapping.contains(Keywords.INDEX)) {

                if (expandedValue instanceof Collection<?> expandedValues) {

                    var list = new ArrayList<Object>(expandedValues.size());

                    for (var item : expandedValues) {
                        list.add(Map.of(Keywords.GRAPH, List.of(item)));
                    }

                    expandedValue = list;

                } else {
                    expandedValue = List.of(Map.of(Keywords.GRAPH, List.of(expandedValue)));
                }
            }

            // 13.13.
            if (keyTermDefinition.filter(TermDefinition::isReverseProperty).isPresent()) {

                if (!(expandedValue instanceof Collection)) {
                    expandedValue = List.of(expandedValue);
                }

                // 13.13.4.
                for (var item : (Collection<?>) expandedValue) {

                    // 13.13.4.1.
                    if (item instanceof Map map &&
                            (JsonLdAdapter.isList(map)
                                    || JsonLdAdapter.isValueNode(map))) {
                        throw new JsonLdException(ErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                    }

                    // 13.13.4.3.
                    var map = result.get(Keywords.REVERSE);

                    if (map == null) {
                        result.put(Keywords.REVERSE, Map.of(expandedProperty, List.of(item)));

                    } else if (map instanceof LinkedHashMap hashmap) {

                        @SuppressWarnings("unchecked")
                        final var typedMap = (Map<String, Object>) hashmap;

                        JsonLdAdapter.setOrAdd(typedMap, expandedProperty, item);

                    } else if (map instanceof Map rawMap) {

                        @SuppressWarnings("unchecked")
                        final var typedMap = (Map<String, Object>) rawMap;

                        final var hashmap = new LinkedHashMap<String, Object>(typedMap);
                        JsonLdAdapter.setOrAdd(hashmap, expandedProperty, item);
                        result.put(Keywords.REVERSE, hashmap);
                    }
                }

            } else {
                // 13.14
                JsonLdAdapter.setOrAdd(result, expandedProperty, expandedValue);
            }

            if (!Keywords.contains(key) || !Keywords.contains(expandedProperty)) {
                params.runtime().fire(EventType.TERM_KEY, key, expandedProperty);
            }

        }

        // 14.
        if (nest != null && !nest.isEmpty()) {
            processNest(activeContext, element);
        }

    }

    public ObjectExpansion1314 nest(Map<String, Object> nest) {
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

    private void keyword(
            final Context activeContext,
            final Object value,
            final String activeProperty,
            final String key,
            final String expandedProperty) throws JsonLdException {

        // 13.4.1
        if (Keywords.REVERSE.equals(activeProperty)) {
            throw new JsonLdException(ErrorCode.INVALID_REVERSE_PROPERTY_MAP);
        }

        // 13.4.2
        if (result.containsKey(expandedProperty)
                && Keywords.noneMatch(expandedProperty, Keywords.INCLUDED, Keywords.TYPE)) {
            throw new JsonLdException(ErrorCode.COLLIDING_KEYWORDS,
                    "Two properties which expand to the same keyword have been detected. A property '" + key + "'"
                            + " expands to '" + expandedProperty + "'"
                            + " but the '" + expandedProperty + "' property is already present.");
        }

        Object expandedValue = null;

        // 13.4.3
        if (Keywords.ID.equals(expandedProperty)) {

            // Extension: JSON-LD-STAR (Experimental)
            if (!params.options().isRdfStar()
                    && Keywords.ANNOTATION.equals(activeProperty)) {
                throw new JsonLdException(ErrorCode.INVALID_ANNOTATION);
            }

            if (params.options().isRdfStar()
                    && value instanceof Map map
                    && !map.isEmpty()) {

                expandedValue = Expansion.expand(activeContext, map, null, key, expandedProperty, params);

                if (!JsonLdAdapter.isEmbedded(expandedValue)) {
                    throw new JsonLdException(ErrorCode.INVALID_EMBEDDED_NODE);
                }

                // 13.4.3.1
            } else if (!params.frameExpansion()
                    && !(value instanceof String)
                    && (!params.options().useNumericId()
                            || !(value instanceof Number)
                            || params.frameExpansion()
                                    && !(value instanceof String)
                                    && !(value instanceof Map map && map.isEmpty())
                                    && (!(value instanceof Collection<?> col)
                                            || col.stream()
                                                    .anyMatch(Predicate.not(e -> !(e instanceof String)))))) {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_ID_VALUE,
                        "An @id entry was encountered whose value [" + value + "] was not a string.");

            } else if (value instanceof String stringValue) {
                // 13.4.3.2
                expandedValue = UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                        .documentRelative(true)
                        .vocab(false)
                        .expand(stringValue);

                if (params.frameExpansion() && expandedValue != null) {
                    expandedValue = List.of(expandedValue);

                } else if (expandedValue == null) {
                    result.put(Keywords.ID, null);
                    return;
                }

            } else if (value instanceof Number numericValue) {

                expandedValue = UriExpansion.with(activeContext, params.options().loader(), params.runtime())
                        .documentRelative(true)
                        .vocab(false)
                        .expand(numericValue.toString());

                if (params.frameExpansion() && expandedValue != null) {
                    expandedValue = List.of(expandedValue);

                } else if (expandedValue == null) {
                    result.put(Keywords.ID, null);
                    return;
                }

            } else if (value instanceof Map) {

                expandedValue = List.of(Map.of());

            } else if (value instanceof Collection<?> col) {

                expandedValue = List.of();

                if (!col.isEmpty()) {
                    List<Object> array = null;

                    for (var item : col) {

                        var expandedStringValue = UriExpansion
                                .with(activeContext, params.options().loader(), params.runtime())
                                .documentRelative(true)
                                .vocab(false)
                                .expand((String) item);

                        if (expandedStringValue != null) {
                            if (array == null) {
                                array = new ArrayList<>();
                            }
                            array.add(expandedStringValue);
                        }
                    }

                    expandedValue = array != null ? array : value;
                }
            }
        }

        // 13.4.4
        else if (Keywords.TYPE.equals(expandedProperty)) {

            // 13.4.4.1
            if ((!params.frameExpansion()
                    && !(value instanceof String)
                    && (!(value instanceof Collection<?> col)
                            || col.stream().anyMatch(Predicate.not(el -> el instanceof String))))
                    || params.frameExpansion()
                            && !(value instanceof String)
                            && !(value instanceof Map map && map.isEmpty())
                            && (!(value instanceof Collection<?> col)
                                    || col.stream().anyMatch(Predicate.not(el -> el instanceof String)))
                            && !JsonLdAdapter.isDefault(value)
                            && JsonLdAdapter.findDefaultValue(value)
                                    .filter(el -> el instanceof String)
                                    .map(String.class::cast)
                                    .map(UriUtils::isNotURI)
                                    .orElse(true)) {

                throw new JsonLdException(ErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + value + "].");
            }

            // 13.4.4.2
            if (value instanceof Map map && map.isEmpty()) {
                expandedValue = Map.of();

                // 13.4.4.3
            } else if (JsonLdAdapter.isDefault(value)) {

                final var defaultValue = JsonLdAdapter.findDefaultValue(value);

                if (defaultValue.filter(el -> el instanceof String).isPresent()) {
                    expandedValue = Map.of(
                            Keywords.DEFAULT,
                            UriExpansion.with(typeContext, params.options().loader(), params.runtime())
                                    .vocab(true)
                                    .documentRelative(true)
                                    // deepcode ignore checkIsPresent~Optional: false positive
                                    .expand(
                                            defaultValue
                                                    .map(String.class::cast)
                                                    .get()));
                }

            } else {
                // 13.4.4.4
                if (value instanceof String valueString) {

                    expandedValue = UriExpansion.with(typeContext, params.options().loader(), params.runtime())
                            .vocab(true)
                            .documentRelative(true)
                            .expand(valueString);

                    if (expandedValue == null) {
                        result.put(Keywords.VALUE, null);
                        return;
                    }

//                    params.runtime().onTypeMapping(Set.of((String)expandedValue));

                } else if (value instanceof Collection col) {

                    List<String> expandedItems = null;

                    for (var item : col) {

                        if (item instanceof String itemString) {

                            final var expandedStringValue = UriExpansion
                                    .with(typeContext, params.options().loader(), params.runtime())
                                    .vocab(true)
                                    .documentRelative(true)
                                    .expand(itemString);

                            if (expandedStringValue != null) {
                                if (expandedItems == null) {
                                    expandedItems = new ArrayList<>();
                                }
                                expandedItems.add(expandedStringValue);
                            }
                        } else {
                            throw new IllegalStateException("TODO");    //TODO
                        }
                    }
                    expandedValue = expandedItems != null
                            ? expandedItems
                            : List.of();

                }

            }

            // 13.4.4.5
            if (result.containsKey(Keywords.TYPE)) {

                var typeValue = result.get(Keywords.TYPE);

                if (typeValue instanceof HashSet<?> set) {
                    @SuppressWarnings("unchecked")
                    var hashset = ((HashSet<Object>) set);
                    hashset.add(expandedValue);
                    expandedValue = set;

                } else if (typeValue instanceof Collection<?> set) {

                    var newSet = new HashSet<Object>(set);
                    newSet.add(expandedValue);
                    expandedValue = newSet;

                } else {
                    expandedValue = List.of(typeValue, expandedValue);
                }
            }
        }

        // 13.4.5
        else if (Keywords.GRAPH.equals(expandedProperty)) {
            expandedValue = asList(
                    Expansion.expand(
                            typeContext,
                            value,
                            Keywords.GRAPH,
                            key,
                            Keywords.GRAPH,
                            params));
        }

        // 13.4.6
        else if (Keywords.INCLUDED.equals(expandedProperty)) {

            // 13.4.6.1
            if (activeContext.isV10()) {
                return;
            }

            // 13.4.6.2
            expandedValue = Expansion
                    .expand(
                            activeContext,
                            value,
                            null,
                            key,
                            expandedProperty,
                            params);

            if (expandedValue != null) {

                if (!(expandedValue instanceof Collection includedValues)) {

                    if (!JsonLdAdapter.isNode(expandedValue)) {
                        throw new JsonLdException(ErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                    }

                    expandedValue = List.of(expandedValue);

                } else {
                    for (var node : includedValues) {
                        if (!JsonLdAdapter.isNode(node)) {
                            throw new JsonLdException(ErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                        }
                    }
                }

                // 13.4.6.4
                final var includedValue = result.get(Keywords.INCLUDED);

                if (includedValue != null) {

                    final List<Object> included;

                    if (includedValue instanceof Collection<?> set) {
                        included = new ArrayList<>(set);
                        included.addAll((Collection<?>) expandedValue);

                    } else {
                        included = new ArrayList<>((Collection<?>) expandedValue);
                        included.add(includedValue);
                    }

                    expandedValue = included;
                }
            } else {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
            }
        }

        // 13.4.7
        if (Keywords.VALUE.equals(expandedProperty)) {

            // 13.4.7.1
            if (Keywords.JSON.equals(inputType)) {

                if (activeContext.isV10()) {
                    throw new JsonLdException(ErrorCode.INVALID_VALUE_OBJECT_VALUE);
                }
                expandedValue = value;
            }
            // 13.4.7.2
            else if (value == null
                    || JsonLdAdapter.isScalar(value)
                    || params.frameExpansion()
                            && ((value instanceof Map map && map.isEmpty())
                                    || (value instanceof Collection<?> col
                                            && (col.isEmpty()
                                                    || col.stream().allMatch(JsonLdAdapter::isScalar))))) {

                expandedValue = value;

                if (params.frameExpansion()) {
                    expandedValue = asList(expandedValue);
                }

            } else {
                throw new JsonLdException(ErrorCode.INVALID_VALUE_OBJECT_VALUE, "An invalid @value=" + value);
            }

            // 13.4.7.4
            if (expandedValue == null) {
                result.put(Keywords.VALUE, null);
                return;
            }
        }

        // 13.4.8
        if (Keywords.LANGUAGE.equals(expandedProperty)) {

            // 13.4.8.1
            if (value instanceof String valueString) {

                if (valueString == null || !LanguageTag.isWellFormed(valueString)) {
                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", valueString);
                }

                // 13.4.8.2
                expandedValue = valueString.toLowerCase();

                if (params.frameExpansion()) {
                    expandedValue = asList(expandedValue);
                }

            } else if (params.frameExpansion() && value instanceof Map map && map.isEmpty()) {

                expandedValue = List.of(Map.of());

            } else if (params.frameExpansion() && value instanceof Collection col && col.isEmpty()) {

                expandedValue = List.of();

            } else if (params.frameExpansion()
                    && value instanceof Collection<?> col
                    && col.stream().allMatch(el -> el instanceof String)) {

                expandedValue = value;

            } else {
                throw new JsonLdException(ErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
            }
        }

        // 13.4.9.
        if (Keywords.DIRECTION.equals(expandedProperty)) {

            // 13.4.9.1.
            if (activeContext.isV10()) {
                return;
            }

            // 13.4.9.2.
            if (value instanceof String valueString
                    && ("ltr".equals(valueString)
                            || "rtl".equals(valueString))) {
                // 13.4.9.3.
                expandedValue = valueString;

                if (params.frameExpansion()) {
                    expandedValue = asList(expandedValue);
                }

            } else if (params.frameExpansion() && value instanceof Map map && map.isEmpty()) {
                expandedValue = List.of(Map.of());

            } else if (params.frameExpansion() && value instanceof Collection col && col.isEmpty()) {
                expandedValue = List.of();

            } else if (params.frameExpansion()
                    && value instanceof Collection<?> col
                    && col.stream().allMatch(el -> el instanceof String)) {
                expandedValue = value;

            } else {
                throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
            }
        }

        // 13.4.10.
        if (Keywords.INDEX.equals(expandedProperty)) {

            // 13.4.10.1.
            if (value instanceof String) {
                // 13.4.10.2
                expandedValue = value;

            } else {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_INDEX_VALUE);
            }
        }

        // 13.4.11
        if (Keywords.LIST.equals(expandedProperty)) {

            // 13.4.11.1
            if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {
                return;
            }

            // 13.4.11.1
            expandedValue = Expansion.expand(
                    activeContext,
                    value,
                    activeProperty,
                    key,
                    expandedProperty,
                    params);

            if (!(expandedValue instanceof Collection<?>)) {
                expandedValue = List.of(expandedValue);
            }
        }

        // 13.4.12
        if (Keywords.SET.equals(expandedProperty)) {
            expandedValue = Expansion.expand(
                    activeContext,
                    value,
                    activeProperty,
                    key,
                    expandedProperty,
                    params);
        }

        // 13.4.13
        if (Keywords.REVERSE.equals(expandedProperty)) {

            // 13.4.13.1.
            if (!(value instanceof Map map)) {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_REVERSE_VALUE);
            }

            // 13.4.13.2.
            expandedValue = Expansion.expand(
                    activeContext,
                    value,
                    Keywords.REVERSE,
                    key,
                    expandedProperty,
                    params);

            if (expandedValue instanceof Map expandedValueObject) {

                // 13.4.13.3.
                if (expandedValueObject.get(Keywords.REVERSE) instanceof Map reverseMap) {

                    final var reverse = (Map<String, ?>) reverseMap;

                    for (var entry : reverse.entrySet()) {
                        // 13.4.13.3.1.
                        JsonLdAdapter.setOrAdd(result, entry.getKey(), entry.getValue());
                    }
                }

                // 13.4.13.4.
                if (expandedValueObject.size() > 1
                        || !expandedValueObject.containsKey(Keywords.REVERSE)) {

                    var reverseMap = new LinkedHashMap<String, Object>();

                    // 13.4.13.4.2
                    @SuppressWarnings("unchecked")
                    final var typedMap = (Map<String, Object>) expandedValueObject;

                    for (var entry : typedMap.entrySet()) {

                        if (Keywords.REVERSE.equals(entry.getKey())) {
                            continue;
                        }

                        // 13.4.13.4.2.1
                        if (entry.getValue() instanceof Collection<?> collection) {

                            for (var item : collection) {

                                // 13.4.13.4.2.1.1
                                if (item instanceof Map<?, ?> itemMap
                                        && (JsonLdAdapter.isList(itemMap)
                                                || JsonLdAdapter.isValueNode(itemMap))) {
                                    throw new JsonLdException(ErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                                }

                                // 13.4.13.4.2.1.1
                                JsonLdAdapter.setOrAdd(reverseMap, entry.getKey(), item);
                            }
                        }
                    }

                    if (!reverseMap.isEmpty()) {
                        result.put(Keywords.REVERSE, reverseMap);
                    }
                }
            }

            // 13.4.13.5.
            return;
        }

        // 13.4.14
        if (Keywords.NEST.equals(expandedProperty)) {

            if (nest == null) {
                nest = new LinkedHashMap<>();
                nest.put(key, List.of());

            } else if (!nest.containsKey(key)) {
                nest.put(key, List.of());
            }

            return;
        }

        // Extension: JSON-LD-STAR (Experimental)
        if (Keywords.ANNOTATION.equals(expandedProperty)) {

            if (!params.options().isRdfStar()) {
                return;
            }

            expandedValue = asList(
                    Expansion.expand(
                            activeContext,
                            value,
                            Keywords.ANNOTATION,
                            key,
                            expandedProperty,
                            params));
        }

        // 13.4.15
        if (params.frameExpansion()
                && (Keywords.DEFAULT.equals(expandedProperty)
                        || Keywords.EMBED.equals(expandedProperty)
                        || Keywords.EXPLICIT.equals(expandedProperty)
                        || Keywords.OMIT_DEFAULT.equals(expandedProperty)
                        || Keywords.REQUIRE_ALL.equals(expandedProperty))) {

            expandedValue = Expansion
                    .expand(activeContext, value, expandedProperty, null, null, params);
        }

        // 13.4.16
        if (expandedValue != null
                || (Keywords.VALUE.equals(expandedProperty)
                        && Keywords.JSON.equals(inputType))) {

            if (Keywords.TYPE.equals(expandedProperty)) {
                if (expandedValue instanceof String typeValue) {
                    params.runtime().fire(EventType.TYPE_KEY, Keywords.TYPE, typeValue);

                } else if (expandedValue instanceof Collection<?> types && types.size() == 1) {

                    params.runtime().fire(EventType.TYPE_KEY, Keywords.TYPE, (String) types.iterator().next());

                } else if (params.runtime().hasValuesConsumer()
                        && expandedValue instanceof Collection<?> types) {

                    params.runtime().fire(EventType.TYPE_KEY, types.stream().map(Object::toString).toList());
                }
            }

            result.put(expandedProperty, expandedValue);
        }
    }

    private void recurse(
            final Context context,
            final Map<String, ?> element,
            final String activeProperty,
            final String term) throws JsonLdException {

        if (activeProperty != null || term != null) {
            params.runtime().fire(EventType.BEGIN_MAP, term != null ? term : activeProperty);
        }

        var activeContext = context;

        // step 3
        var propertyContext = activeContext
                .findTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        // step 8
        if (propertyContext != null) {
            activeContext = activeContext
                    .newContext(params.options().loader(), params.runtime())
                    .overrideProtected(true)
                    .build(
                            propertyContext,
                            activeContext
                                    .findTerm(activeProperty)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null));

            // TODO contextKeys?
        }

        // steps 13-14
        expand(activeContext, element, activeProperty, term);

        if (activeProperty != null || term != null) {
            params.runtime().fire(EventType.END_MAP, term != null ? term : activeProperty);
        }

    }

    private void processNest(
            final Context activeContext,
            final Map<String, ?> element) throws JsonLdException {

        final var nestedKeys = params.options().isOrdered()
                ? nest.keySet().stream().sorted().iterator()
                : nest.keySet().iterator();

        while (nestedKeys.hasNext()) {

            final var nestedKey = nestedKeys.next();

            final var nestedNode = element.get(nestedKey);

            final boolean isCollection;
            final Collection<?> collection;

            if (nestedNode instanceof Collection<?> col) {
                isCollection = true;
                collection = col;
            } else {
                isCollection = false;
                collection = List.of(nestedNode);
            }

            var counter = 0;

            // 14.2.
            for (final var nestValue : collection) {

                // 14.2.1
                if (!(nestValue instanceof Map nestedMap)) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_NEST_VALUE);
                }

                for (final var nestedValueKey : nestedMap.keySet()) {
                    if (Keywords.VALUE.equals(
                            UriExpansion.with(typeContext, params.options().loader(), params.runtime())
                                    .vocab(true)
                                    .expand((String) nestedValueKey))) {
                        throw new JsonLdException(ErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                }

                if (isCollection) {
                    params.runtime().fire(EventType.BEGIN_LIST, nestedKey);
                }

                // 14.2.2
                ObjectExpansion1314
                        .with(params)
                        .inputType(inputType)
                        .result(result)
                        .typeContext(typeContext)
                        .nest(new LinkedHashMap<>())
                        .recurse(
                                activeContext,
                                nestedMap,
                                nestedKey,
                                isCollection
                                        ? Integer.toString(counter++)
                                        : nestedKey);

                if (isCollection) {
                    params.runtime().fire(EventType.END_LIST, nestedKey);
                }
            }

        }
    }

    private static Collection<?> asList(Object value) {
        if (value instanceof Collection<?> collection) {
            return List.copyOf(collection);
        }
        return List.of(value);
    }
}
