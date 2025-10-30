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
package com.apicatalog.jsonld.compaction;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdAdapter;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.processor.ProcessingRuntime;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#compaction-algorithm">Compaction
 *      Algorithm</a>
 *
 */
public final class Compaction {

    private static final Collection<String> DILV_KEYWORDS = Arrays.asList(
            Keywords.DIRECTION,
            Keywords.INDEX,
            Keywords.LANGUAGE,
            Keywords.VALUE);

    // required
    private final Context context;
    private final ProcessingRuntime runtime;

    // optional
    private boolean compactArrays;
    private boolean ordered;

    private Compaction(final Context context, final ProcessingRuntime runtime) {
        this.context = context;
        this.runtime = runtime;

        // default values
        this.compactArrays = false;
        this.ordered = false;
    }

    public static Compaction with(final Context activeContext, final ProcessingRuntime runtime) {
        return new Compaction(activeContext, runtime);
    }

    public Compaction compactArrays(final boolean compactArrays) {
        this.compactArrays = compactArrays;
        return this;
    }

    public Compaction ordered(final boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public Object compact(final Object element) throws JsonLdException, IOException {
        return compact(null, element);
    }

    public Object compact(final String activeProperty, final Object element) throws JsonLdException, IOException {

        if (element instanceof Collection<?> array) {
            return compactArray(activeProperty, array);
        }

        if (element instanceof Map map) {
            return compactObject(activeProperty, (Map<String, ?>) map);
        }

        // return scalar
        return element;
    }

    public Object compactArray(final String activeProperty, final Collection<?> array) throws JsonLdException, IOException {

        final var activePropertyDefinition = context.findTerm(activeProperty);

        // 3.1.
        final var result = new ArrayList<Object>();

        // 3.2.
        for (final var item : array) {

            // 3.2.1.
            final var compactedItem = Compaction
                    .with(context, runtime)
                    .compactArrays(compactArrays)
                    .ordered(ordered)
                    .compact(activeProperty, item);

            // 3.2.2.
            if (compactedItem != null) {
                result.add(compactedItem);
            }
        }

        // 3.3.
        if (result.isEmpty()
                || result.size() > 1
                || !compactArrays
                || Keywords.GRAPH.equals(activeProperty)
                || Keywords.SET.equals(activeProperty)
                || activePropertyDefinition
                        .filter(d -> d.hasContainerMapping(Keywords.LIST)
                                || d.hasContainerMapping(Keywords.SET))
                        .isPresent()) {

            return result;
        }

        // 3.4.
        return result.get(0);
    }

    public Object compactObject(final String activeProperty, final Map<String, ?> object) throws JsonLdException, IOException {

        // 1.
        Context typeContext = context;
        Context activeContext = context;

        final Optional<TermDefinition> activePropertyDefinition = activeContext.findTerm(activeProperty);

        // 5.
        if (activeContext.getPreviousContext() != null
                && !object.containsKey(Keywords.VALUE)
                && !(object.containsKey(Keywords.ID)
                        && object.size() == 1)) {
            activeContext = activeContext.getPreviousContext();
        }

        // 6. 
        
        if (activePropertyDefinition.map(TermDefinition::getLocalContext).isPresent()) {

            var localContext = activePropertyDefinition.get().getLocalContext();

            activeContext = activeContext
                    .newContext(runtime.getDocumentLoader())
                    .overrideProtected(true)
                    .build(localContext.node(),
                            localContext.adapter(),
                            activePropertyDefinition.get().getBaseUrl());
        }

        // 7.
        if ((object.containsKey(Keywords.VALUE) || object.containsKey(Keywords.ID))
                && (!runtime.isRdfStar() || !object.containsKey(Keywords.ANNOTATION))) {

            final var result = ValueCompaction.compact(activeContext, object, activeProperty);

            if (result != null && NativeAdapter.instance().type(result).isScalar()
                    || activePropertyDefinition
                            .map(TermDefinition::getTypeMapping)
                            .filter(Keywords.JSON::equals)
                            .isPresent()) {

                return result;
            }
        }

        // 8.
        if (JsonLdAdapter.isList(object)
                && activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.LIST)).isPresent()) {

            return Compaction
                    .with(activeContext, runtime)
                    .compactArrays(compactArrays)
                    .ordered(ordered)
                    .compact(activeProperty, object.get(Keywords.LIST));
        }

        // 9.
        final var insideReverse = Keywords.REVERSE.equals(activeProperty);

        // 10.
        final var result = new LinkedHashMap<String, Object>();

        // 11.
        if (object.containsKey(Keywords.TYPE)) {

            final List<String> compactedTypes = new ArrayList<>();

            for (final var type : NativeAdapter.asCollection(object.get(Keywords.TYPE))) {
                compactedTypes.add(UriCompaction.withVocab(activeContext, (String) type));
            }

            Collections.sort(compactedTypes);

            for (final var term : compactedTypes) {

                final var termDefinition = typeContext.findTerm(term).orElse(null);

                // 11.1.
                if (termDefinition != null && termDefinition.getLocalContext() != null) {

                    final var localContext = termDefinition.getLocalContext();

                    activeContext = activeContext
                            .newContext(runtime.getDocumentLoader())
                            .propagate(false)
                            .build(
                                    localContext.node(),
                                    localContext.adapter(),
                                    termDefinition.getBaseUrl());
                }
            }
        }

        final var keys = ordered
                ? object.keySet().stream().sorted().iterator()
                : object.keySet().iterator();

        // 12.
        while (keys.hasNext()) {

            final var expandedProperty = keys.next();

            final var expandedValue = object.get(expandedProperty);

            // 12.1.
            if (Keywords.ID.equals(expandedProperty)) {

                Object compactedValue = null;

                // 12.1.1.
                if (expandedValue instanceof String stringValue) {
                    compactedValue = UriCompaction.compact(activeContext, stringValue);

//FIXME                    // json-ld-star
//                } else if (activeContext.runtime().isRdfStar()
//                        && JsonLdNode.isEmbedded(expandedValue)) {
//                    compactedValue = Compaction.with(activeContext)
//                            .compactArrays(compactArrays)
//                            .ordered(ordered)
//                            .compact(expandedValue);
                }

                // 12.1.3.
                result.put(
                        UriCompaction.withVocab(activeContext, expandedProperty),
                        compactedValue);

                continue;
            }

            // 12.2.
            if (Keywords.TYPE.equals(expandedProperty)) {

                final Object compactedValue;

                // 12.2.1.
                if (expandedValue instanceof String stringValue) {
                    compactedValue = UriCompaction.withVocab(typeContext, stringValue);

                    // 12.2.2.
                } else if (expandedValue instanceof Collection<?> arrayValue) {

                    // 12.2.2.1.
                    final var compactedArray = new ArrayList<String>(arrayValue.size());

                    // 12.2.2.2.
                    for (final var expandedType : arrayValue) {

                        compactedArray.add(UriCompaction.withVocab(typeContext, (String) expandedType));
                    }

                    compactedValue = compactedArray;

                } else {
                    throw new JsonLdException(JsonLdErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + expandedValue + "].");
                }

                // 12.2.3.
                final var alias = UriCompaction.withVocab(activeContext, expandedProperty);

                // 12.2.4.
                final boolean asArray = !compactArrays
                        || (activeContext.isV11()
                                && activeContext.findTerm(alias).filter(t -> t.hasContainerMapping(Keywords.SET)).isPresent());

                // 12.2.5.
                JsonLdAdapter.setOrAdd(result, alias, compactedValue, asArray);

                // 12.2.6.
                continue;
            }

            // 12.3.
            if (Keywords.REVERSE.equals(expandedProperty)) {

                // 12.3.1.
                final var compactedMap = (Map<String, ?>) Compaction
                        .with(activeContext, runtime)
                        .compactArrays(compactArrays)
                        .ordered(ordered)
                        .compact(Keywords.REVERSE, expandedValue);

                Map<String, Object> remaining = null;

                // 12.3.2.
                for (final var entry : compactedMap.entrySet()) {

                    // 12.3.2.1.
                    if (activeContext
                            .findTerm(entry.getKey())
                            .filter(TermDefinition::isReverseProperty)
                            .isPresent()) {

                        // 12.3.2.1.1
                        final boolean asArray = !compactArrays
                                || activeContext
                                        .findTerm(entry.getKey())
                                        .filter(td -> td.hasContainerMapping(Keywords.SET))
                                        .isPresent();

                        // 12.3.2.1.2.
                        JsonLdAdapter.setOrAdd(result, entry.getKey(), entry.getValue(), asArray);

                    } else {

                        if (remaining == null) {
                            remaining = new HashMap<>();
                        }

                        remaining.put(entry.getKey(), entry.getValue());
                    }

                }

                // 12.8.3.
                if (remaining != null) {

                    // 12.8.3.2.
                    result.put(
                            UriCompaction.withVocab(activeContext, Keywords.REVERSE),
                            remaining);
                }

                // 12.8.4.
                continue;
            }

            // 12.4.
            if (Keywords.PRESERVE.equals(expandedProperty)) {

                // 12.4.1.
                final var compactedValue = Compaction
                        .with(activeContext, runtime)
                        .compactArrays(compactArrays)
                        .ordered(ordered)
                        .compact(activeProperty, expandedValue);

                // 12.4.2.
                if (!(compactedValue instanceof Collection array) || !array.isEmpty()) {
//                if (!JsonUtils.isEmptyArray(compactedValue)) {
                    result.put(Keywords.PRESERVE, compactedValue);
                }
                continue;
            }

            // 12.5.
            if (Keywords.INDEX.equals(expandedProperty)
                    && activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.INDEX)).isPresent()) {
                continue;

                // 12.6.
            } else if (DILV_KEYWORDS.contains(expandedProperty)) {

                // 12.6.2.
                result.put(
                        UriCompaction.withVocab(activeContext, expandedProperty),
                        expandedValue);

                continue;
            }

            // 12.7.
            if (expandedValue instanceof Collection<?> array && array.isEmpty()) {

                // 12.7.1.
                final var itemActiveProperty = UriCompaction.withVocab(
                        activeContext,
                        expandedProperty,
                        expandedValue,
                        insideReverse);

                // 12.7.2.
                final Optional<String> nestProperty = activeContext
                        .findTerm(itemActiveProperty)
                        .map(TermDefinition::getNestValue);

                if (nestProperty.isPresent()) {

                    final String nestTerm = nestProperty.get();

                    // 12.7.2.1.
                    if (!Keywords.NEST.equals(nestTerm) && !Keywords.NEST.equals(activeContext
                            .uriExpansion(runtime.getDocumentLoader())
                            .vocab(true)
                            .expand(nestTerm))) {
                        throw new JsonLdException(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }

                    // 12.7.2.3.
                    JsonLdAdapter.setOrAdd(
                            (Map<String, Object>) result.computeIfAbsent(nestTerm, k -> new LinkedHashMap<String, Object>()),
                            itemActiveProperty,
                            List.of());

//                    result.getMapBuilder(nestTerm).add(itemActiveProperty, List.of());

                    // 12.7.3.
                } else {
                    JsonLdAdapter.setOrAdd(result, itemActiveProperty, List.of());
                }

            }
//System.out.println(expandedValue); .asJsonArray()
            // 12.8.
            for (final var expandedItem : (Collection<?>) expandedValue) {

                // 12.8.1.
                final var itemActiveProperty = UriCompaction.withVocab(
                        activeContext,
                        expandedProperty,
                        expandedItem,
                        insideReverse);

                Map<String, Object> nestResult = null;
                String nestResultKey = null;

                // 12.8.2.
                final Optional<String> nestProperty = activeContext
                        .findTerm(itemActiveProperty)
                        .map(TermDefinition::getNestValue);

                if (nestProperty.isPresent()) {

                    final String nestTerm = nestProperty.get();

                    // 12.8.2.1.
                    if (!Keywords.NEST.equals(nestTerm) 
                            && !Keywords.NEST.equals(activeContext
                                    .uriExpansion(runtime.getDocumentLoader())
                                    .vocab(true)
                                    .expand(nestTerm))) {
                        throw new JsonLdException(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }

                    // 12.8.2.3.
                    // TODO ?!?!?
                    nestResult = (Map<String, Object>) result.computeIfAbsent(nestTerm, k -> new LinkedHashMap<>());
//                    nestResult = result.getMapBuilder(nestTerm);                    
                    nestResultKey = nestTerm;

                    // 12.8.3.
                } else {
                    nestResult = result;
                }

                // 12.8.4.
                final Collection<String> container = activeContext
                        .findTerm(itemActiveProperty)
                        .map(TermDefinition::getContainerMapping)
                        .orElse(List.of());

                // 12.8.5.
                final boolean asArray = container.contains(Keywords.SET)
                        || Keywords.GRAPH.equals(itemActiveProperty)
                        || Keywords.LIST.equals(itemActiveProperty)
                        || !compactArrays;

                // 12.8.6.
                var expandedItemValue = expandedItem;

                if (expandedItem instanceof Map map) {
                    if (JsonLdAdapter.isList(map)) {
                        expandedItemValue = map.get(Keywords.LIST);

                    } else if (JsonLdAdapter.isGraph(map)) {
                        expandedItemValue = map.get(Keywords.GRAPH);
                    }
                }

                Object compactedItem = Compaction
                        .with(activeContext, runtime)
                        .compactArrays(compactArrays)
                        .ordered(ordered)
                        .compact(itemActiveProperty, expandedItemValue);

                // 12.8.7.
                if (expandedItem instanceof Map expandedItemMap
                        && JsonLdAdapter.isList(expandedItemMap)) {

                    // 12.8.7.1.
                    compactedItem = NativeAdapter.asCollection(compactedItem);

                    // 12.8.7.2.
                    if (!container.contains(Keywords.LIST)) {

                        // 12.8.7.2.1.
                        final var key = UriCompaction.withVocab(activeContext, Keywords.LIST);

                        // 12.8.7.2.2.
                        if (expandedItemMap.containsKey(Keywords.INDEX)) {

                            final var indexKey = UriCompaction.withVocab(activeContext, Keywords.INDEX);

                            compactedItem = Map.of(
                                    key, compactedItem,
                                    indexKey, expandedItemMap.get(Keywords.INDEX));

                        } else {
                            compactedItem = Map.of(key, compactedItem);

                        }

                        // 12.8.7.2.3.
                        JsonLdAdapter.setOrAdd(nestResult, itemActiveProperty, compactedItem, asArray);

                        // 12.8.7.3.
                    } else {
                        nestResult.put(itemActiveProperty, compactedItem);
                    }

                    // 12.8.8.
                } else if (expandedItem instanceof Map expandedItemMap && JsonLdAdapter.isGraph(expandedItemMap)) {

                    boolean followup = false;

                    // 12.8.8.1.
                    if (container.contains(Keywords.GRAPH) && container.contains(Keywords.ID)) {

                        // 12.8.8.1.2.
                        String mapKey = null;

                        var id = expandedItemMap.get(Keywords.ID);

                        if (id instanceof String idString) {

                            mapKey = UriCompaction.compact(activeContext, idString);

                        } else if (id == null) {
                            mapKey = UriCompaction.withVocab(activeContext, Keywords.NONE);

                        } else {
                            throw new IllegalStateException();
                        }

                        // 12.8.8.1.3.
//                        nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);

                        JsonLdAdapter.setOrAdd(
                                (Map<String, Object>) nestResult.computeIfAbsent(itemActiveProperty, k -> new LinkedHashMap<String, Object>()),
                                mapKey,
                                compactedItem,
                                asArray);

                        // 12.8.8.2.
                    } else if (container.contains(Keywords.GRAPH)
                            && container.contains(Keywords.INDEX)
                            && JsonLdAdapter.isSimpleGraph(expandedItemMap)) {

                        // 12.8.8.2.2.
                        final String mapKey = expandedItemMap.containsKey(Keywords.INDEX)
                                ? (String) expandedItemMap.get(Keywords.INDEX)
                                : Keywords.NONE;

                        // 12.8.8.2.3.
//                        nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);

                        JsonLdAdapter.setOrAdd(
                                (Map<String, Object>) nestResult
                                        .computeIfAbsent(itemActiveProperty, k -> new LinkedHashMap<String, Object>()),
                                mapKey,
                                compactedItem,
                                asArray);

                        // 12.8.8.3.
                    } else if (container.contains(Keywords.GRAPH)
                            && JsonLdAdapter.isSimpleGraph(expandedItemMap)) {

                        // 12.8.8.3.1.
                        if (compactedItem instanceof Collection array && array.size() > 1) {
                            compactedItem = Map.of(
                                    UriCompaction.withVocab(activeContext, Keywords.INCLUDED),
                                    compactedItem);
                        }

                        // 12.8.8.3.2.
                        JsonLdAdapter.setOrAdd(nestResult, itemActiveProperty, compactedItem, asArray);

                    } else {
                        followup = true;
                    }

                    // 12.8.8.4.
                    if (!container.contains(Keywords.GRAPH) || followup) {

                        final Map<String, Object> compactedItemMap;

                        // 12.8.8.4.2.
                        if (expandedItemMap.containsKey(Keywords.ID)) {
                            compactedItemMap = Map.of(
                                    UriCompaction.withVocab(activeContext, Keywords.GRAPH),
//                                            .uriCompaction()
//                                            .vocab(true)
//                                            .compact(Keywords.GRAPH),
                                    compactedItem,
                                    UriCompaction.withVocab(activeContext, Keywords.ID),
//                                            .uriCompaction()
//                                            .vocab(true)
//                                            .compact(Keywords.ID),
                                    UriCompaction.compact(activeContext, (String) expandedItemMap.get(Keywords.ID)));
//                                            .uriCompaction()
//                                            .compact((String) expandedItemMap.get(Keywords.ID)));

                        } else {
                            // 12.8.8.4.1.
                            compactedItemMap = Map.of(
                                    UriCompaction.withVocab(activeContext, Keywords.GRAPH),
//                                            .uriCompaction()
//                                            .vocab(true)
//                                            .compact(Keywords.GRAPH),
                                    compactedItem);
                        }

                        compactedItem = compactedItemMap;

                        // 12.8.8.4.3.
                        if (expandedItemMap.containsKey(Keywords.INDEX)) {

                            var map = new LinkedHashMap<>(compactedItemMap);
                            map.put(
                                    UriCompaction.withVocab(activeContext, Keywords.INDEX),
//                                            .uriCompaction()
//                                            .vocab(true)
//                                            .compact(Keywords.INDEX),
                                    (String) expandedItemMap.get(Keywords.INDEX));
                            compactedItem = map;
                        }

                        // 12.8.8.4.4.
                        JsonLdAdapter.setOrAdd(nestResult, itemActiveProperty, compactedItem, asArray);
                    }

                    // 12.8.9.
                } else if ((container.contains(Keywords.LANGUAGE)
                        || container.contains(Keywords.INDEX)
                        || container.contains(Keywords.ID)
                        || container.contains(Keywords.TYPE))
                        && !container.contains(Keywords.GRAPH)) {

                    // 12.8.9.2.
                    String keyToCompact = null;

                    if (container.contains(Keywords.LANGUAGE)) {
                        keyToCompact = Keywords.LANGUAGE;

                    } else if (container.contains(Keywords.INDEX)) {
                        keyToCompact = Keywords.INDEX;

                    } else if (container.contains(Keywords.ID)) {
                        keyToCompact = Keywords.ID;

                    } else if (container.contains(Keywords.TYPE)) {
                        keyToCompact = Keywords.TYPE;
                    }

                    var containerKey = UriCompaction.withVocab(activeContext, keyToCompact);

                    // 12.8.9.3.
                    final var indexKey = activeContext
                            .findTerm(itemActiveProperty)
                            .map(TermDefinition::getIndexMapping)
                            .orElse(Keywords.INDEX);

                    String mapKey = null;

                    // 12.8.9.4.
                    if (container.contains(Keywords.LANGUAGE)
                            && expandedItem instanceof Map expandedItemMap
                            && expandedItemMap.containsKey(Keywords.VALUE)) {

                        if (compactedItem instanceof Map compactedItemMap) {
                            compactedItem = compactedItemMap.get(Keywords.VALUE);
                        }

                        if (expandedItemMap.containsKey(Keywords.LANGUAGE)) {
                            mapKey = (String) expandedItemMap.get(Keywords.LANGUAGE);
                        }

                        // 12.8.9.5.
                    } else if (container.contains(Keywords.INDEX)
                            && expandedItem instanceof Map expandedItemMap
                            && Keywords.INDEX.equals(indexKey)) {

                        if (expandedItemMap.containsKey(Keywords.INDEX)) {

                            mapKey = (String) expandedItemMap.get(Keywords.INDEX);
                        }

                        // 12.8.9.6.
                    } else if (container.contains(Keywords.INDEX) && !Keywords.INDEX.equals(indexKey)) {

                        // 12.8.9.6.1.
                        containerKey = UriCompaction.withVocab(activeContext,
                                activeContext.uriExpansion(runtime.getDocumentLoader()).expand(indexKey));

                        // 12.8.9.6.2.
                        if (compactedItem instanceof Map<?, ?> compactedItemMap
                                && compactedItemMap.containsKey(containerKey)) {
//                        if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            final var containerValue = compactedItemMap.get(containerKey);

                            if (containerValue instanceof String valueString) {
                                mapKey = valueString;

                                var newMap = new LinkedHashMap<>(compactedItemMap);
                                newMap.remove(containerKey);

                                // 12.8.9.6.3.
                                compactedItem = newMap;

//                                        compactedItem.asJsonObject()).remove(containerKey).build();

                            } else if (containerValue instanceof Collection<?> listValue
                                    && !listValue.isEmpty()) {
//                            } else if (JsonUtils.isArray(containerValue) && !JsonUtils.isEmptyArray(containerValue)) {

                                var it = listValue.iterator();

                                mapKey = (String) it.next();

                                if (listValue.size() == 1) {

                                    compactedItem = new LinkedHashMap<>(compactedItemMap);
                                    ((Map) compactedItem).remove(containerKey);
                                    // containerValue.asJsonArray().getString(0);
                                } else {

                                    Object containerKeyValue;

                                    if (listValue.size() == 2) {
                                        containerKeyValue = it.next();

                                    } else {
                                        var list = new ArrayList<>(listValue.size() - 1);
                                        it.forEachRemaining(list::add);
                                        containerKeyValue = list;
                                    }

                                    compactedItem = new LinkedHashMap<>(compactedItemMap);
                                    ((Map<?, ?>) compactedItem).remove(containerKey);
                                    ((Map) compactedItem).put(containerKey, containerKeyValue);

//                                }
//                                
//                                // 12.8.9.6.3.
//                                if (listValue.size() > 1) {
//
//                                    
//                                    var containerKeyValue = it.next();
//
//                                    if (it.hasNext()) {
//                                    }

//                                    if (listValue.size() == 2) {
//                                        containerKeyValue = containerValue.asJsonArray().get(1);
//
//                                    } else {
//                                        containerKeyValue = JsonProvider.instance().createArrayBuilder(containerValue.asJsonArray()).remove(0).build();
//                                    }

//                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject())
//                                            .remove(containerKey)
//                                            .add(containerKey, containerKeyValue)
//                                            .build();

//                                } else {
//                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                                }
                            }
                        }

                        // 12.8.9.7.
                    } else if (container.contains(Keywords.ID)) {

                        if (compactedItem instanceof Map map && map.containsKey(containerKey)) {
                            // if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            mapKey = (String) map.get(containerKey);

                            compactedItem = new LinkedHashMap<>(map);
                            ((Map<?, ?>) compactedItem).remove(containerKey);
                        }

                        // 12.8.9.8.
                    } else if (container.contains(Keywords.TYPE)) {

                        // 12.8.9.8.1.
                        if (compactedItem instanceof Map map && map.containsKey(containerKey)) {
//                        if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            final var compactedKeyValue = map.get(containerKey);

                            if (compactedKeyValue != null) {

                                final var compactedKeyArray = NativeAdapter.asCollection(compactedKeyValue);

                                var it = compactedKeyArray.iterator();

                                mapKey = (String) it.next();

                                if (compactedKeyArray.size() > 1) {

                                    Object compactedKeyArrayValue = null;

                                    if (compactedKeyArray.size() == 2) {

                                        compactedKeyArrayValue = it.next();

                                    } else {

                                        var list = new ArrayList<>(compactedKeyArray.size() - 1);
                                        it.forEachRemaining(list::add);
                                        compactedKeyArrayValue = list;
//                                                JsonProvider.instance().createArrayBuilder(compactedKeyArray).remove(0).build();
                                    }

                                    compactedItem = new LinkedHashMap<String, Object>(map);
                                    ((Map<?, ?>) compactedItem).remove(containerKey);
                                    ((Map) compactedItem).put(containerKey, compactedKeyArrayValue);

                                } else {
                                    compactedItem = new LinkedHashMap<String, Object>(map);
                                    ((Map<?, ?>) compactedItem).remove(containerKey);
                                }

                            } else {
                                compactedItem = new LinkedHashMap<String, Object>(map);
                                ((Map<?, ?>) compactedItem).remove(containerKey);
                            }
                        }

                        // 12.8.9.8.4.
                        if (compactedItem instanceof Map map && map.size() == 1) {
//                        if (JsonUtils.isObject(compactedItem) && compactedItem.asJsonObject().size() == 1) {

                            final var expandedKey = activeContext
                                    .uriExpansion(runtime.getDocumentLoader())
                                    .vocab(true)
                                    .expand((String) map.keySet().iterator().next());

                            if (Keywords.ID.equals(expandedKey)) {
                                compactedItem = Compaction
                                        .with(activeContext, runtime)
                                        .compact(itemActiveProperty,
                                                Map.of(
                                                        Keywords.ID,
                                                        ((Map) expandedItem).get(Keywords.ID)));
                            }
                        }
                    }

                    // 12.8.9.9.
                    if (mapKey == null) {
                        mapKey = UriCompaction.withVocab(activeContext, Keywords.NONE);
                    }

                    // 12.8.9.10.
//                    nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);
//                    System.out.println(">>>>> " + nestResult.get(itemActiveProperty));
                    JsonLdAdapter.setOrAdd(
                            (Map<String, Object>) nestResult.computeIfAbsent(itemActiveProperty, k -> new LinkedHashMap<String, Object>()),
                            mapKey,
                            compactedItem,
                            asArray);

                    // 12.8.10.
                } else {
                    JsonLdAdapter.setOrAdd(nestResult, itemActiveProperty, compactedItem, asArray);
                }

                if (nestResult != null && nestResultKey != null) {
                    result.put(nestResultKey, nestResult);
                }
            }
        }

        // 13.
        return result;
    }
}