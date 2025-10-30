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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.expansion.Expansion.Params;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.tree.io.java.NativeMaterializer3;

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

    // optional
    private Params params;

    private ObjectExpansion1314(Params params) {
        this.params = params;
    }

    public static ObjectExpansion1314 with(Params params) {
        return new ObjectExpansion1314(params);
    }

    public void expand(
            final Context activeContext,
            final Object element,
            final NodeAdapter adapter,
            final String activeProperty) throws JsonLdError, IOException {

        var keys = params.ordered()
                ? adapter.keyStream(element).sorted(PolyNode.comparingElement(adapter::asString)).iterator()
                : adapter.keyStream(element).iterator();

        // 13.
        while (keys.hasNext()) {

            var key = adapter.asString(keys.next());

            // 13.1.
            if (Keywords.CONTEXT.equals(key)) {
                continue;
            }

            params.runtime().tick();

            // 13.2.
            var expandedProperty = activeContext
                    .uriExpansion(params.runtime().getDocumentLoader())
                    .documentRelative(false)
                    .vocab(true)
                    .expand(key);

            // if the term is undefined and
            if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
                switch (params.runtime().getUndefinedTermPolicy()) {
                case Fail:
                    throw new JsonLdError(JsonLdErrorCode.UNDEFINED_TERM,
                            "An undefined term has been found [" + key + "]. Change policy to Ignore or Warn or define the term in a context");
                case Warn:
                    LOGGER.log(Level.WARNING, "An undefined term has been found [{0}]", key);

                case Ignore:
                    continue;
                }
            }

            final var value = adapter.property(key, element);

            // 13.4. If expanded property is a keyword:
            if (Keywords.contains(expandedProperty)) {

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

                Object expandedValue = null;

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

                    if (!params.frameExpansion()
                            && !adapter.isString(value)
                            && (!params.runtime().isNumericId()
                                    || !adapter.isNumber(value))
                            || params.frameExpansion()
                                    && !adapter.isString(value)
                                    && !adapter.isEmptyMap(value)
                                    && (!adapter.isCollection(value)
                                            || adapter.elementStream(value)
                                                    .anyMatch(Predicate.not(adapter::isString)))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE, "An @id entry was encountered whose value [" + value + "] was not a string.");

                    } else if (adapter.isString(value)) {
                        // 13.4.3.2
                        expandedValue = activeContext
                                .uriExpansion(params.runtime().getDocumentLoader())
                                .documentRelative(true)
                                .vocab(false)
                                .expand(adapter.stringValue(value));

                        if (params.frameExpansion() && expandedValue != null) {
                            expandedValue = List.of(expandedValue);

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

                    } else if (adapter.isNumber(value)) {

//                        final String expandedStringValue = activeContext
                        expandedValue = activeContext
                                .uriExpansion(params.runtime().getDocumentLoader())
                                .documentRelative(true)
                                .vocab(false)
                                .expand(adapter.asString(value));

                        if (params.frameExpansion() && expandedValue != null) {
                            expandedValue = List.of(expandedValue);

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

                    } else if (adapter.isMap(value)) {

                        expandedValue = List.of(Map.of());

                    } else if (adapter.isEmptyCollection(value)) {

                        expandedValue = List.of();

                    } else if (adapter.isCollection(value)) {

                        List<Object> array = null;

                        for (var item : adapter.asIterable(value)) {

                            String expandedStringValue = activeContext
                                    .uriExpansion(params.runtime().getDocumentLoader())
                                    .documentRelative(true)
                                    .vocab(false)
                                    .expand(adapter.stringValue(item));

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

                // 13.4.4
                else if (Keywords.TYPE.equals(expandedProperty)) {
                    // 13.4.4.1
                    if ((!params.frameExpansion()
                            && !adapter.isString(value)
                            && (!adapter.isCollection(value)
                                    || adapter.elementStream(value)
                                            .anyMatch(Predicate.not(adapter::isString))))
                            || params.frameExpansion()
                                    && !adapter.isString(value)
                                    && !adapter.isEmptyMap(value)
                                    && (!adapter.isCollection(value)
                                            || adapter.elementStream(value)
                                                    .anyMatch(Predicate.not(adapter::isString)))
                                    && !JsonLdAdapter.isDefault(value, adapter)
                                    && JsonLdAdapter.findDefaultValue(value, adapter)
                                            .filter(adapter::isString)
                                            .map(adapter::stringValue)
                                            .map(UriUtils::isNotURI)
                                            .orElse(true)) {

                        throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + value + "].");
                    }

                    // 13.4.4.2
                    if (adapter.isEmptyMap(value)) {
                        expandedValue = Map.of();

                        // 13.4.4.3
                    } else if (JsonLdAdapter.isDefault(value, adapter)) {

                        final var defaultValue = JsonLdAdapter.findDefaultValue(value, adapter);

                        if (defaultValue.filter(adapter::isString).isPresent()) {
                            expandedValue = Map.of(
                                    Keywords.DEFAULT,
                                    typeContext
                                            .uriExpansion(params.runtime().getDocumentLoader())
                                            .vocab(true)
                                            .documentRelative(true)
                                            // deepcode ignore checkIsPresent~Optional: false positive
                                            .expand(defaultValue
                                                    .map(adapter::stringValue)
                                                    .get()));
                        }

                    } else {
                        // 13.4.4.4
                        if (adapter.isString(value)) {

                            expandedValue = typeContext
                                    .uriExpansion(params.runtime().getDocumentLoader())
                                    .vocab(true)
                                    .documentRelative(true)
                                    .expand(adapter.stringValue(value));

                            if (expandedValue == null) {
                                result.put(Keywords.VALUE, null);
                                continue;
                            }

                        } else if (adapter.isCollection(value)) {

                            List<String> expandedItems = null;

                            final var items = adapter.elementStream(value)
                                    .filter(adapter::isString)
                                    .map(adapter::stringValue)
                                    .iterator();

                            while (items.hasNext()) {

                                final var item = items.next();

                                final String expandedStringValue = typeContext
                                        .uriExpansion(params.runtime().getDocumentLoader())
                                        .vocab(true)
                                        .documentRelative(true)
                                        .expand(item);

                                if (expandedStringValue != null) {
                                    if (expandedItems == null) {
                                        expandedItems = new ArrayList<>();
                                    }
                                    expandedItems.add(expandedStringValue);
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

                        if (typeValue instanceof HashSet set) {
                            @SuppressWarnings("unchecked")
                            var hashset = ((HashSet<Object>) set);
                            hashset.add(expandedValue);
                            expandedValue = set;

                        } else if (typeValue instanceof Collection<?> set) {

                            var newSet = (new HashSet<Object>(set));
                            newSet.add(expandedValue);
                            expandedValue = newSet;

                        } else {
                            expandedValue = List.of(typeValue, expandedValue);
                        }
                    }
                }

                // 13.4.5
                else if (Keywords.GRAPH.equals(expandedProperty)) {
                    expandedValue = asList(Expansion.expand(
                            typeContext,
                            value,
                            adapter,
                            Keywords.GRAPH,
                            params));
                }

                // 13.4.6
                else if (Keywords.INCLUDED.equals(expandedProperty)) {

                    // 13.4.6.1
                    if (activeContext.isV10()) {
                        continue;
                    }

                    // 13.4.6.2
                    expandedValue = Expansion
                            .expand(activeContext, value, adapter, null, params);

                    if (expandedValue != null) {

                        if (!(expandedValue instanceof Collection)) {
                            expandedValue = List.of(expandedValue);
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
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                    }
                }

                // 13.4.7
                if (Keywords.VALUE.equals(expandedProperty)) {

                    // 13.4.7.1
                    if (Keywords.JSON.equals(inputType)) {

                        if (activeContext.isV10()) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
                        }

                        expandedValue = NativeMaterializer3.node(value, adapter); // FIXMEnew PolyNode(value, adapter);

                        // 13.4.7.2
                    } else if (adapter.isNull(value)
                            || adapter.type(value).isScalar()
                            || params.frameExpansion()
                                    && (adapter.isEmptyMap(value)
                                            || adapter.isEmptyCollection(value)
                                            || adapter.isCollection(value)
                                                    && adapter.elementStream(value)
                                                            .map(adapter::type)
                                                            .allMatch(NodeType::isScalar))) {

                        expandedValue = NativeMaterializer3.node(value, adapter);

                        if (params.frameExpansion()) {
                            expandedValue = asList(expandedValue);
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
                    if (adapter.isString(value)) {

                        var stringValue = adapter.stringValue(value);

                        if (stringValue == null || !LanguageTag.isWellFormed(stringValue)) {
                            LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", stringValue);
                        }

                        // 13.4.8.2
                        expandedValue = stringValue.toLowerCase();

                        if (params.frameExpansion()) {
                            expandedValue = asList(expandedValue);
                        }

                    } else if (params.frameExpansion() && adapter.isEmptyMap(value)) {

                        expandedValue = List.of(Map.of());

                    } else if (params.frameExpansion() && adapter.isEmptyCollection(value)) {

                        expandedValue = List.of();

                    } else if (params.frameExpansion() && adapter.isCollection(value)
                            && adapter
                                    .elementStream(value)
                                    .allMatch(adapter::isString)) {

                        expandedValue = adapter.elementStream(value).map(adapter::stringValue).toList();

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                    }
                }

                // 13.4.9.
                if (Keywords.DIRECTION.equals(expandedProperty)) {

                    // 13.4.9.1.
                    if (activeContext.isV10()) {
                        continue;
                    }

                    // 13.4.9.2.
                    if (adapter.isString(value)
                            && ("ltr".equals(adapter.stringValue(value))
                                    || "rtl".equals(adapter.stringValue(value)))) {

                        // 13.4.9.3.
                        expandedValue = adapter.stringValue(value);

                        if (params.frameExpansion()) {
                            expandedValue = asList(expandedValue);
                        }

                    } else if (params.frameExpansion() && adapter.isEmptyMap(value)) {

                        expandedValue = List.of(Map.of());

                    } else if (params.frameExpansion() && adapter.isEmptyCollection(value)) {

                        expandedValue = List.of();

                    } else if (params.frameExpansion() && adapter.isCollection(value)
                            && adapter
                                    .elementStream(value)
                                    .allMatch(adapter::isString)) {

                        expandedValue = adapter.elementStream(value).map(adapter::stringValue).toList();

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    }
                }

                // 13.4.10.
                if (Keywords.INDEX.equals(expandedProperty)) {

                    // 13.4.10.1.
                    if (adapter.isString(value)) {
                        // 13.4.10.2
                        expandedValue = adapter.stringValue(value);

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
                    expandedValue = Expansion.expand(
                            activeContext,
                            value,
                            adapter,
                            activeProperty,
                            params);

                    if (!(expandedValue instanceof Collection<?>)) {
                        expandedValue = List.of(expandedValue);
                    }
                }

                // 13.4.12
                if (Keywords.SET.equals(expandedProperty)) {
                    expandedValue = Expansion
                            .expand(activeContext, value, adapter, activeProperty, params);
                }

                // 13.4.13
                if (Keywords.REVERSE.equals(expandedProperty)) {

                    // 13.4.13.1.
                    if (!adapter.isMap(value)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_REVERSE_VALUE);
                    }

                    // 13.4.13.2.
                    expandedValue = Expansion.expand(
                            activeContext,
                            value,
                            adapter,
                            Keywords.REVERSE,
                            params);

                    if (expandedValue instanceof Map expandedValueObject) {

                        // 13.4.13.3.
                        if (expandedValueObject.get(Keywords.REVERSE) instanceof Map map) {

                            @SuppressWarnings("unchecked")
                            final var reverse = (Map<String, Object>) map;

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
                            for (var entry : ((Map<String, Object>) expandedValueObject).entrySet()) {

                                if (Keywords.REVERSE.equals(entry.getKey())) {
                                    continue;
                                }

                                // 13.4.13.4.2.1
                                if (entry.getValue() instanceof Collection<?> collection) {

                                    for (var item : collection) {

                                        // 13.4.13.4.2.1.1
                                        if (item instanceof Map<?, ?> map
                                                && (JsonLdAdapter.isList(map)
                                                        || JsonLdAdapter.isValueNode(map))) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
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
                    continue;
                }

                // 13.4.14
                if (Keywords.NEST.equals(expandedProperty)) {

                    if (nest == null) {
                        nest = new LinkedHashMap<>();
                        nest.put(key, List.of());

                    } else if (!nest.containsKey(key)) {
                        nest.put(key, List.of());
                    }

                    continue;
                }

                // Extension: JSON-LD-STAR (Experimental)
                if (Keywords.ANNOTATION.equals(expandedProperty)) {

                    if (!params.runtime().isRdfStar()) {
                        continue;
                    }

                    expandedValue = asList(Expansion.expand(
                            activeContext,
                            value,
                            adapter,
                            Keywords.ANNOTATION,
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
                            .expand(activeContext, value, adapter, expandedProperty, params);
                }

                // 13.4.16
                if (expandedValue != null
                        || (Keywords.VALUE.equals(expandedProperty)
                                && Keywords.JSON.equals(inputType))) {

                    result.put(expandedProperty, expandedValue);
                }

                // 13.4.17
                continue;
            }

            // 13.5.
            final Optional<TermDefinition> keyTermDefinition = activeContext.findTerm(key);

            final Collection<String> containerMapping = keyTermDefinition
                    .map(TermDefinition::getContainerMapping)
                    .orElse(List.of());

            Object expandedValue = null;

            // 13.6.
            if (keyTermDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.JSON::equals)
                    .isPresent()) {

//                (JsonStructure) new JakartaMaterializer().node(collection, new JsonLdAdapter())

//                expandedValue = new NativeMaterializer().node(value, JakartaAdapter.instance());
                expandedValue = new PolyNode(value, JakartaAdapter.instance());

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
            } else if (adapter.isMap(value) && containerMapping.contains(Keywords.LANGUAGE)) {

                // 13.7.1.
//                expandedValue = JsonValue.EMPTY_JSON_ARRAY;
                final var langMaps = new ArrayList<>();

                // 13.7.2.
                final var direction = keyTermDefinition
                        .map(TermDefinition::getDirectionMapping)
                        .orElseGet(() -> activeContext.getDefaultBaseDirection());

                // 13.7.4.
                final var langCodes = params.ordered()
                        ? adapter.keyStream(value).sorted().iterator()
                        : adapter.keyStream(value).iterator();

                while (langCodes.hasNext()) {

                    final var langCode = langCodes.next();
                    final var langValue = adapter.property(langCode, value);

                    // 13.7.4.2.
                    for (final var item : adapter.asIterable(langValue)) {

                        // 13.7.4.2.1.
                        if (adapter.isNull(item)) {
                            continue;
                        }

                        // 13.7.4.2.2.
                        if (!adapter.isString(item)) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAP_VALUE);
                        }

                        final var langMap = new LinkedHashMap<>();
                        langMap.put(Keywords.VALUE, adapter.stringValue(item));

                        // 13.7.4.2.4.
                        if (!Keywords.NONE.equals(langCode)) {

                            final String expandedLangCode = activeContext
                                    .uriExpansion(params.runtime().getDocumentLoader())
                                    .vocab(true)
                                    .expand((String) langCode);

                            if (!Keywords.NONE.equals(expandedLangCode)) {

                                if (!LanguageTag.isWellFormed((String) langCode)) {
                                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", langCode);
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
//                        expandedValue = JsonProvider.instance().createArrayBuilder(expandedValue.asJsonArray()).add(langMap).build();
                    }
                }

                expandedValue = Set.copyOf(langMaps);

            } else if (adapter.isMap(value)
                    && (containerMapping.contains(Keywords.INDEX)
                            || containerMapping.contains(Keywords.TYPE)
                            || containerMapping.contains(Keywords.ID))) {

                // 13.8.1.
                final var indices = new ArrayList<>();

                // 13.8.2.
                final var indexKey = keyTermDefinition
                        .map(TermDefinition::getIndexMapping)
                        .orElse(Keywords.INDEX);

                final var valueKeys = params.ordered()
                        ? adapter.keyStream(value).sorted().iterator()
                        : adapter.keys(value).iterator();

                // 13.8.3.
                while (valueKeys.hasNext()) {

                    final var valueKey = valueKeys.next();

                    final var indexValue = adapter.property(valueKey, value);

                    final var index = adapter.asString(valueKey);

                    // 13.8.3.1.
                    Context mapContext = activeContext;

                    if (activeContext.getPreviousContext() != null
                            && ((containerMapping.contains(Keywords.ID) && !containerMapping.contains(Keywords.SET)) || containerMapping.contains(Keywords.TYPE))) {
                        mapContext = activeContext.getPreviousContext();
                    }

                    // 13.8.3.2.
                    var indexTermDefinition = mapContext.findTerm(index).orElse(null);

                    if (containerMapping.contains(Keywords.TYPE)
                            && indexTermDefinition != null
                            && indexTermDefinition.getLocalContext() != null) {

                        mapContext = mapContext
                                .newContext(params.runtime().getDocumentLoader())
                                .build(
                                        indexTermDefinition.getLocalContext().node(),
                                        indexTermDefinition.getLocalContext().adapter(),
                                        indexTermDefinition.getBaseUrl());
                    }

                    // 13.8.3.3.
                    if (mapContext == null) {
                        mapContext = activeContext;
                    }

                    // 13.8.3.4.
                    final var expandedIndex = activeContext
                            .uriExpansion(params.runtime().getDocumentLoader())
                            .vocab(true)
                            .expand(index);

                    // 13.8.3.6.
                    final var indexValues = Expansion.array(
                            mapContext,
                            indexValue,
                            adapter,
                            key,
                            new Params(params.frameExpansion(), params.ordered(), true, params.baseUrl(), params.runtime()));

                    // 13.8.3.7.
                    for (final var item : indexValues) {

                        final var indexMap = new LinkedHashMap<String, Object>();

                        // 13.8.3.7.1.
                        if (containerMapping.contains(Keywords.GRAPH) && JsonLdAdapter.isNotGraph(item)) {
                            indexMap.put(Keywords.GRAPH, List.of(item));

                        } else {
                            indexMap.putAll((Map) item);
                        }

                        // 13.8.3.7.2.
                        if (containerMapping.contains(Keywords.INDEX)
                                && !Keywords.INDEX.equals(indexKey)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.2.1.
                            var reExpandedIndex = ValueExpansion.expand(
                                    activeContext,
                                    indexKey,
                                    index,
                                    NativeAdapter.instance(),
                                    params.runtime());

                            // 13.8.3.7.2.2.
                            var expandedIndexKey = activeContext
                                    .uriExpansion(params.runtime().getDocumentLoader())
                                    .vocab(true)
                                    .expand(indexKey);

                            // 13.8.3.7.2.3.
                            var indexPropertyValues = new ArrayList<>();
                            indexPropertyValues.add(reExpandedIndex);

                            var existingValues = indexMap.get(expandedIndexKey);

                            if (existingValues instanceof Collection<?> values) {
                                indexPropertyValues.addAll(values);
//FIXME?                                    existingValues
//                                            .stream()
//                                            .map(JsonUtils::asScalar)
//                                            .forEach(indexPropertyValues::add);

                            } else if (existingValues != null) {
                                indexPropertyValues.add(existingValues);
                            }

                            // 13.8.3.7.2.4.
                            indexMap.put(expandedIndexKey, indexPropertyValues);

                            // 13.8.3.7.2.5.
                            if (item instanceof Map map
                                    && JsonLdAdapter.isValueNode(map)
                                    && indexMap.size() > 1) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
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
                            indexMap.put(Keywords.ID, activeContext
                                    .uriExpansion(params.runtime().getDocumentLoader())
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
                expandedValue = indices;

                // 13.9.
            } else {
                expandedValue = Expansion.expand(
                        activeContext,
                        value,
                        adapter,
                        key,
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
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                    }

                    // 13.13.4.3.
                    var map = result.get(Keywords.REVERSE);

                    if (map == null) {
                        result.put(Keywords.REVERSE, Map.of(expandedProperty, List.of(item)));
                    } else if (map instanceof LinkedHashMap hashmap) {

                        JsonLdAdapter.setOrAdd(hashmap, expandedProperty, item);

                    } else if (map instanceof Map rawMap) {
                        var hashmap = new LinkedHashMap<String, Object>(rawMap);
                        JsonLdAdapter.setOrAdd(hashmap, expandedProperty, item);
                        result.put(Keywords.REVERSE, hashmap);
                    }
                }

            } else {
                // 13.14
                JsonLdAdapter.setOrAdd(result, expandedProperty, expandedValue);
            }
        }

        // 14.
        if (nest != null)

        {
            processNest(activeContext, element, adapter);
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

    private void recurse(
            final Context context,
            final Object element,
            final NodeAdapter adapter,
            final String activeProperty) throws JsonLdError, IOException {

        var activeContext = context;

        params.runtime().tick();

        // step 3
        var propertyContext = activeContext
                .findTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);

        // step 8
        if (propertyContext != null) {
            activeContext = activeContext
                    .newContext(params.runtime().getDocumentLoader())
                    .overrideProtected(true)
                    .build(
                            propertyContext.node(),
                            propertyContext.adapter(),
                            activeContext
                                    .findTerm(activeProperty)
                                    .map(TermDefinition::getBaseUrl)
                                    .orElse(null));
        }

        // steps 13-14
        expand(activeContext, element, adapter, activeProperty);
    }

    private void processNest(
            final Context activeContext,
            final Object element,
            final NodeAdapter adapter) throws JsonLdError, IOException {

        final var nestedKeys = params.ordered()
                ? nest.keySet().stream().sorted().iterator()
                : nest.keySet().iterator();

        while (nestedKeys.hasNext()) {

            final var nestedKey = nestedKeys.next();

            // 14.2.
            for (final var nestValue : adapter.asIterable(adapter.property(nestedKey, element))) {

                // 14.2.1
                if (!adapter.isMap(nestValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                }

                for (final var nestedValueKey : adapter.keys(nestValue)) {
                    if (Keywords.VALUE.equals(
                            typeContext.uriExpansion(params.runtime().getDocumentLoader())
                                    .vocab(true)
                                    .expand(adapter.asString(nestedValueKey)))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }
                }

                // 14.2.2
                ObjectExpansion1314
                        .with(params)
                        .inputType(inputType)
                        .result(result)
                        .typeContext(typeContext)
                        .nest(new LinkedHashMap<>())
                        .recurse(activeContext, nestValue, adapter, nestedKey);
            }
        }
    }
//
//    @SuppressWarnings({ "unchecked", "rawtypes" })
//    private static void merge(Map map, String key, Object value) {
//
//        var previous = map.get(key);
//
//        if (previous == null) {
//            if (value instanceof Collection<?>) {
//                map.put(key, value);
//                return;
//            }
//            map.put(key, Set.of(value));
//            return;
//        }
//
//        if (previous instanceof Collection<?> c1) {
//
//            final Collection<Object> result;
//
//            if (previous instanceof ArrayList list) {
//                result = list;
//            } else {
//                result = new ArrayList<Object>(c1);
//            }
//
//            if (value instanceof Collection<?> c2) {
//                result.addAll(c2);
//
//            } else {
//                result.add(value);
//            }
//
//            map.put(key, result);
//            return;
//        }
//
//        map.put(key, List.of(previous, value));
//    }

    private static Collection<?> asList(Object value) {
        if (value instanceof Collection<?> collection) {
            return List.copyOf(collection);
        }
        return List.of(value);
    }
}
