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

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonMapBuilder;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DefaultObject;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.GraphObject;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.Utils;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
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
    private ActiveContext activeContext;

    private final JsonObject element;
    private final String activeProperty;
    private final URI baseUrl;

    private ActiveContext typeContext;
    private JsonMapBuilder result;
    private String inputType;
    private Map<String, JsonValue> nest;

    // optional
    private boolean frameExpansion;
    private boolean ordered;

    private ObjectExpansion1314(final ActiveContext activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
    }

    public static final ObjectExpansion1314 with(final ActiveContext activeContext, final JsonObject element,
            final String activeProperty, final URI baseUrl) {
        return new ObjectExpansion1314(activeContext, element, activeProperty, baseUrl);
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

    public ObjectExpansion1314 typeContext(ActiveContext typeContext) {
        this.typeContext = typeContext;
        return this;
    }

    public ObjectExpansion1314 result(JsonMapBuilder result) {
        this.result = result;
        return this;
    }

    public ObjectExpansion1314 inputType(String inputType) {
        this.inputType = inputType;
        return this;
    }

    public void expand() throws JsonLdError {

        // 13.
        for (final String key : Utils.index(element.keySet(), ordered)) {

            // 13.1.
            if (Keywords.CONTEXT.equals(key)) {
                continue;
            }

            // 13.2.
            String expandedProperty =
                        activeContext
                            .uriExpansion()
                            .documentRelative(false)
                            .vocab(true)
                            .expand(key);

            // 13.3.
            if (expandedProperty == null || (!expandedProperty.contains(":") && !Keywords.contains(expandedProperty))) {
                continue;
            }

            JsonValue value = element.get(key);

            // 13.4. If expanded property is a keyword:
            if (Keywords.contains(expandedProperty)) {

                JsonValue expandedValue = null;

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
                                + " but the '" + expandedProperty + "' property is already present." );
                }

                // 13.4.3
                if (Keywords.ID.equals(expandedProperty)) {

                    // Extension: JSON-LD-STAR (Experimental)
                    if (!activeContext.getOptions().isRdfStar() && Keywords.ANNOTATION.equals(activeProperty)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_ANNOTATION);

                    } else if (activeContext.getOptions().isRdfStar() && JsonUtils.isNonEmptyObject(value)) {

                        expandedValue = Expansion
                                            .with(activeContext, value, null, baseUrl)
                                            .frameExpansion(frameExpansion)
                                            .ordered(ordered)
                                            .compute();

                        if (!NodeObject.isEmbeddedNode(expandedValue)) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_EMBEDDED_NODE);
                        }

                    // 13.4.3.1
                    } else if (!frameExpansion && JsonUtils.isNotString(value) && (!activeContext.getOptions().isNumericId() || JsonUtils.isNotNumber(value))
                            || frameExpansion
                                    && JsonUtils.isNotString(value)
                                    && JsonUtils.isNonEmptyObject(value)
                                    && (JsonUtils.isNotArray(value)
                                            || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString)
                                    )
                            ) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE, "An @id entry was encountered whose value [" + value + "] was not a string.");

                    // 13.4.3.2
                    } else if (JsonUtils.isString(value)) {

                        final String expandedStringValue =
                                    activeContext
                                        .uriExpansion()
                                        .documentRelative(true)
                                        .vocab(false)
                                        .expand(((JsonString) value).getString());

                        if (expandedStringValue != null) {

                            expandedValue = Json.createValue(expandedStringValue);

                            if (frameExpansion) {
                                expandedValue = Json.createArrayBuilder().add(expandedValue).build();
                            }
                        } else {
                            expandedValue = JsonValue.NULL;
                        }

                    } else if (JsonUtils.isNumber(value)) {

                        final String expandedStringValue =
                                    activeContext
                                        .uriExpansion()
                                        .documentRelative(true)
                                        .vocab(false)
                                        .expand(((JsonNumber) value).toString());

                        if (expandedStringValue != null) {

                            expandedValue = Json.createValue(expandedStringValue);

                            if (frameExpansion) {
                                expandedValue = Json.createArrayBuilder().add(expandedValue).build();
                            }
                        } else {
                            expandedValue = JsonValue.NULL;
                        }

                    } else if (JsonUtils.isObject(value)) {

                        expandedValue = Json.createArrayBuilder().add(JsonValue.EMPTY_JSON_OBJECT).build();

                    } else if (JsonUtils.isEmptyArray(value)) {

                        expandedValue = JsonValue.EMPTY_JSON_ARRAY;

                    } else if (JsonUtils.isArray(value))  {

                        final JsonArrayBuilder array = Json.createArrayBuilder();

                        for (final JsonValue item : JsonUtils.toCollection(value)) {

                            String expandedStringValue =
                                    activeContext
                                        .uriExpansion()
                                        .documentRelative(true)
                                        .vocab(false)
                                        .expand(((JsonString) item).getString());

                            if (expandedStringValue != null) {
                                array.add(expandedStringValue);
                            }
                        }

                        expandedValue = array.build();
                    }
                }

                // 13.4.4
                else if (Keywords.TYPE.equals(expandedProperty)) {

                    // 13.4.4.1
                    if ((!frameExpansion
                            && JsonUtils.isNotString(value)
                            && (JsonUtils.isNotArray(value)
                                    || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString)
                                ))
                            || frameExpansion
                                    && JsonUtils.isNotString(value)
                                    && JsonUtils.isNonEmptyObject(value)
                                    && (JsonUtils.isNotArray(value)
                                            || value.asJsonArray().stream().anyMatch(JsonUtils::isNotString)
                                        )
                                    && !DefaultObject.isDefaultObject(value)
                                    && DefaultObject.getValue(value)
                                                    .filter(JsonUtils::isString)
                                                    .map(JsonString.class::cast)
                                                    .map(JsonString::getString)
                                                    .map(UriUtils::isNotURI)
                                                    .orElse(true)
                            ) {

                        throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + value + "].");
                    }

                    // 13.4.4.2
                    if (JsonUtils.isEmptyObject(value)) {
                        expandedValue = value;

                    // 13.4.4.3
                    } else if (DefaultObject.isDefaultObject(value)) {

                        final Optional<JsonValue> defaultValue = DefaultObject.getValue(value);

                        if (defaultValue.filter(JsonUtils::isString).isPresent()) {

                            expandedValue = Json.createObjectBuilder()
                                    .add(Keywords.DEFAULT,
                                            typeContext
                                                    .uriExpansion()
                                                    .vocab(true)
                                                    .documentRelative(true)
                                                    // deepcode ignore checkIsPresent~Optional: false positive
                                                    .expand(defaultValue.map(JsonString.class::cast).map(JsonString::getString).get()))
                                    .build();
                        }

                    // 13.4.4.4
                    } else {

                        if (JsonUtils.isString(value)) {

                            String expandedStringValue =
                                        typeContext
                                            .uriExpansion()
                                            .vocab(true)
                                            .documentRelative(true)
                                            .expand(((JsonString) value).getString());

                            if (expandedStringValue != null) {
                                expandedValue = Json.createValue(expandedStringValue);
                            }

                        } else if (JsonUtils.isArray(value)) {

                            final JsonArrayBuilder array = Json.createArrayBuilder();

                            for (final JsonValue item : value.asJsonArray()) {

                                if (JsonUtils.isString(item)) {

                                    final String expandedStringValue =
                                            typeContext
                                                .uriExpansion()
                                                .vocab(true)
                                                .documentRelative(true)
                                                .expand(((JsonString) item).getString());

                                    if (expandedStringValue != null) {
                                        array.add(Json.createValue(expandedStringValue));
                                    }
                                }
                            }
                            expandedValue = array.build();
                        }
                    }

                    // 13.4.4.5
                    if (result.containsKey(Keywords.TYPE)) {

                        final JsonValue typeValue = result.get(Keywords.TYPE).orElse(null);

                        if (JsonUtils.isArray(typeValue)) {
                            expandedValue = Json.createArrayBuilder(typeValue.asJsonArray()).add(expandedValue).build();

                        } else {
                            expandedValue = Json.createArrayBuilder().add(typeValue).add(expandedValue).build();
                        }
                    }
                }

                // 13.4.5
                else if (Keywords.GRAPH.equals(expandedProperty)) {

                    expandedValue = JsonUtils.toJsonArray(Expansion
                                        .with(typeContext, value, Keywords.GRAPH, baseUrl)
                                        .frameExpansion(frameExpansion)
                                        .ordered(ordered)
                                        .compute()
                                        );
                }

                // 13.4.6
                else if (Keywords.INCLUDED.equals(expandedProperty)) {

                    // 13.4.6.1
                    if (activeContext.inMode(JsonLdVersion.V1_0)) {
                        continue;
                    }

                    // 13.4.6.2
                    expandedValue = Expansion
                                        .with(activeContext, value, null, baseUrl)
                                        .frameExpansion(frameExpansion)
                                        .ordered(ordered)
                                        .compute();

                    if (JsonUtils.isNotNull(expandedValue)) {

                        if (JsonUtils.isNotArray(expandedValue)) {
                            expandedValue = Json.createArrayBuilder().add(expandedValue).build();
                        }

                        // 13.4.6.3
                        if (expandedValue.asJsonArray().stream().anyMatch(NodeObject::isNotNodeObject)) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                        }

                        // 13.4.6.4
                        if (result.containsKey(Keywords.INCLUDED)) {

                            final JsonValue includedValue = result.get(Keywords.INCLUDED).orElse(null);

                            final JsonArrayBuilder included;

                            if (JsonUtils.isArray(includedValue)) {
                                included = Json.createArrayBuilder(includedValue.asJsonArray());

                            } else {
                                included = Json.createArrayBuilder().add(includedValue);
                            }

                            expandedValue.asJsonArray().forEach(included::add);

                            expandedValue = included.build();
                        }
                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INCLUDED_VALUE);
                    }
                }

                // 13.4.7
                if (Keywords.VALUE.equals(expandedProperty)) {

                    // 13.4.7.1
                    if (Keywords.JSON.equals(inputType)) {

                        if (activeContext.inMode(JsonLdVersion.V1_0)) {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
                        }

                        expandedValue = value;

                    // 13.4.7.2
                    } else if (JsonUtils.isNull(value)
                                || JsonUtils.isScalar(value)
                                    || frameExpansion
                                       && (JsonUtils.isEmptyObject(value)
                                               || JsonUtils.isEmptyArray(value)
                                               || JsonUtils.isArray(value)
                                                       && value.asJsonArray().stream().allMatch(JsonUtils::isScalar)
                                               )
                            ) {

                        expandedValue = value;

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toJsonArray(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT_VALUE);
                    }

                    // 13.4.7.4
                    if (JsonUtils.isNull(expandedValue)) {
                        result.put(Keywords.VALUE, JsonValue.NULL);
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
                                                && value.asJsonArray().stream().allMatch(JsonUtils::isString)
                                        )
                            ) {

                        if (JsonUtils.isString(value) && !LanguageTag.isWellFormed(((JsonString)value).getString())) {
                            LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", ((JsonString)value).getString());
                        }

                        // 13.4.8.2
                        expandedValue = JsonUtils.isString(value) ? Json.createValue(((JsonString)value).getString().toLowerCase()) : value;

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toJsonArray(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                    }
                }

                // 13.4.9.
                if (Keywords.DIRECTION.equals(expandedProperty)) {
                    // 13.4.9.1.
                    if (activeContext.inMode(JsonLdVersion.V1_0)) {
                        continue;
                    }

                    // 13.4.9.2.
                    if ((JsonUtils.isString(value)
                            && "ltr".equals(((JsonString) value).getString()) || "rtl".equals(((JsonString) value).getString()))
                            || frameExpansion
                                    && (JsonUtils.isEmptyObject(value)
                                            || JsonUtils.isEmptyArray(value)
                                            || JsonUtils.isArray(value)
                                                    && value.asJsonArray().stream().allMatch(JsonUtils::isString)
                                            )
                            ) {

                        // 13.4.9.3.
                        expandedValue = value;

                        if (frameExpansion) {
                            expandedValue = JsonUtils.toJsonArray(expandedValue);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    }
                }

                // 13.4.10.
                if (Keywords.INDEX.equals(expandedProperty)) {

                    // 13.4.10.1.
                    if (JsonUtils.isNotString(value)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_INDEX_VALUE);
                    }

                    // 13.4.10.2
                    expandedValue = value;
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

                    if (JsonUtils.isNotArray(expandedValue)) {
                        expandedValue = Json.createArrayBuilder().add(expandedValue).build();
                    }
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

                    if (JsonUtils.isObject(expandedValue)) {

                        final JsonObject expandedValueObject = expandedValue.asJsonObject();

                        // 13.4.13.3.
                        if (expandedValueObject.containsKey(Keywords.REVERSE)) {

                            for (final Entry<String, JsonValue> entry : expandedValueObject.get(Keywords.REVERSE).asJsonObject().entrySet()) {
                                // 13.4.13.3.1.
                                result.add(entry.getKey(), entry.getValue());
                            }
                        }

                        // 13.4.13.4.
                        if (expandedValueObject.size() > 1
                                || !expandedValueObject.containsKey(Keywords.REVERSE)) {


                            final JsonMapBuilder reverseMap = result.getMapBuilder(Keywords.REVERSE);

                            // 13.4.13.4.2
                            for (final Entry<String, JsonValue> entry : expandedValueObject.entrySet()) {

                                if (Keywords.REVERSE.equals(entry.getKey())) {
                                    continue;
                                }

                                // 13.4.13.4.2.1
                                if (JsonUtils.isArray(entry.getValue())) {

                                    for (final JsonValue item : entry.getValue().asJsonArray()) {

                                        // 13.4.13.4.2.1.1
                                        if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                                        }

                                        // 13.4.13.4.2.1.1
                                        reverseMap.add(entry.getKey(), item);
                                    }
                                }
                            }

                            if (reverseMap.isEmpty()) {
                                result.remove(Keywords.REVERSE);
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

                    if (!activeContext.getOptions().isRdfStar()) {
                        continue;
                    }

                    expandedValue = JsonUtils.toJsonArray(Expansion
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
                                || Keywords.REQUIRE_ALL.equals(expandedProperty))
                        ) {

                    expandedValue = Expansion.with(activeContext, value, expandedProperty, baseUrl)
                                            .frameExpansion(frameExpansion)
                                            .ordered(ordered)
                                            .compute();
                }

                // 13.4.16
                if (expandedValue != null
                        || (Keywords.VALUE.equals(expandedProperty) && Keywords.JSON.equals(inputType))) {

                    result.put(expandedProperty, expandedValue);
                }

                // 13.4.17
                continue;
            }

            // 13.5.
            final Optional<TermDefinition> keyTermDefinition = activeContext.getTerm(key);

            final Collection<String> containerMapping = keyTermDefinition
                                                        .map(TermDefinition::getContainerMapping)
                                                        .orElseGet(() -> Collections.emptyList());

            JsonValue expandedValue = null;

            // 13.6.
            if (keyTermDefinition.map(TermDefinition::getTypeMapping).filter(Keywords.JSON::equals).isPresent()) {

                expandedValue = Json.createObjectBuilder().add(Keywords.VALUE, value)
                        .add(Keywords.TYPE, Keywords.JSON).build();

            // 13.7.
            } else if (containerMapping.contains(Keywords.LANGUAGE) && JsonUtils.isObject(value)) {

                // 13.7.1.
                expandedValue = JsonValue.EMPTY_JSON_ARRAY;

                // 13.7.2.
                final DirectionType direction = keyTermDefinition
                                                    .map(TermDefinition::getDirectionMapping)
                                                    .orElseGet(() -> activeContext.getDefaultBaseDirection())
                                                    ;

                final JsonObject valueObject = value.asJsonObject();

                // 13.7.4.
                for (final String langCode : Utils.index(valueObject.keySet(), ordered)) {

                    JsonValue langValue = valueObject.get(langCode);

                    // 13.7.4.1.
                    if (JsonUtils.isNotArray(langValue)) {
                        langValue = Json.createArrayBuilder().add(langValue).build();
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
                        final JsonObjectBuilder langMap = Json.createObjectBuilder().add(Keywords.VALUE, item);

                        // 13.7.4.2.4.
                        if (!Keywords.NONE.equals(langCode)) {

                            final String expandedLangCode =
                                                activeContext
                                                    .uriExpansion()
                                                    .vocab(true)
                                                    .expand(langCode);

                            if (!Keywords.NONE.equals(expandedLangCode)) {

                                if (!LanguageTag.isWellFormed(langCode)) {
                                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", langCode);
                                }

                                langMap.add(Keywords.LANGUAGE, Json.createValue(langCode.toLowerCase()));
                            }
                        }

                        // 13.7.4.2.5.
                        if (direction != null && !DirectionType.NULL.equals(direction)) {
                            langMap.add(Keywords.DIRECTION, Json.createValue(direction.name().toLowerCase()));
                        }

                        // 13.7.4.2.6.
                        expandedValue = Json.createArrayBuilder(expandedValue.asJsonArray()).add(langMap).build();
                    }
                }

            // 13.8.
            } else if ((containerMapping.contains(Keywords.INDEX) || containerMapping.contains(Keywords.TYPE)
                    || containerMapping.contains(Keywords.ID)) && JsonUtils.isObject(value)) {

                // 13.8.1.
                expandedValue = JsonValue.EMPTY_JSON_ARRAY;

                // 13.8.2.
                final String indexKey = keyTermDefinition
                                            .map(TermDefinition::getIndexMapping)
                                            .orElse(Keywords.INDEX);

                // 13.8.3.
                for (final String index : Utils.index(value.asJsonObject().keySet(), ordered)) {

                    JsonValue indexValue = value.asJsonObject().get(index);

                    // 13.8.3.1.
                    ActiveContext mapContext = activeContext;

                    if (activeContext.getPreviousContext() != null
                            && (containerMapping.contains(Keywords.ID) || containerMapping.contains(Keywords.TYPE))) {

                        mapContext = activeContext.getPreviousContext();
                    }

                    // 13.8.3.2.
                    final Optional<TermDefinition> indexTermDefinition = mapContext.getTerm(index);

                    if (containerMapping.contains(Keywords.TYPE)
                                && indexTermDefinition.map(TermDefinition::getLocalContext).isPresent()
                                ) {

                        mapContext =
                                mapContext
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
                    String expandedIndex =
                                activeContext
                                    .uriExpansion()
                                    .vocab(true)
                                    .expand(index);

                    // 13.8.3.5.
                    if (JsonUtils.isNotArray(indexValue)) {
                        indexValue = Json.createArrayBuilder().add(indexValue).build();
                    }

                    // 13.8.3.6.
                    indexValue = Expansion.with(mapContext, indexValue, key, baseUrl).fromMap(true)
                            .frameExpansion(frameExpansion).ordered(ordered).compute();

                    // 13.8.3.7.
                    for (JsonValue item : indexValue.asJsonArray()) {

                        // 13.8.3.7.1.
                        if (containerMapping.contains(Keywords.GRAPH) && !GraphObject.isGraphObject(item)) {
                            item = GraphObject.toGraphObject(item);
                        }

                        // 13.8.3.7.2.
                        if (containerMapping.contains(Keywords.INDEX) && !Keywords.INDEX.equals(indexKey)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            // 13.8.3.7.2.1.
                            JsonValue reExpandedIndex = activeContext.valueExpansion()
                                    .expand(Json.createValue(index), indexKey);

                            // 13.8.3.7.2.2.
                            final String expandedIndexKey =
                                        activeContext
                                            .uriExpansion()
                                            .vocab(true)
                                            .expand(indexKey);

                            // 13.8.3.7.2.3.
                            final JsonArrayBuilder indexPropertyValues = Json.createArrayBuilder().add(reExpandedIndex);

                            final JsonValue existingValues = item.asJsonObject().get(expandedIndexKey);

                            if (JsonUtils.isNotNull(existingValues)) {
                                if (JsonUtils.isArray(existingValues)) {
                                    existingValues.asJsonArray().forEach(indexPropertyValues::add);

                                } else {
                                    indexPropertyValues.add(existingValues);
                                }
                            }

                            // 13.8.3.7.2.4.
                            item = Json.createObjectBuilder(item.asJsonObject())
                                    .add(expandedIndexKey, indexPropertyValues).build();


                            // 13.8.3.7.2.5.
                            if (ValueObject.isValueObject(item) && item.asJsonObject().size() > 1) {
                                throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
                            }

                        // 13.8.3.7.3.
                        } else if (containerMapping.contains(Keywords.INDEX)
                                && !item.asJsonObject().containsKey(Keywords.INDEX)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.INDEX, index).build();

                        // 13.8.3.7.4.
                        } else if (containerMapping.contains(Keywords.ID)
                                && !item.asJsonObject().containsKey(Keywords.ID)
                                && !Keywords.NONE.equals(expandedIndex)) {

                            expandedIndex = activeContext
                                                .uriExpansion()
                                                .vocab(false)
                                                .documentRelative(true)
                                                .expand(index);

                            item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.ID, expandedIndex)
                                    .build();

                        // 13.8.3.7.5.
                        } else if (containerMapping.contains(Keywords.TYPE) && !Keywords.NONE.equals(expandedIndex)) {

                            final JsonArrayBuilder types = Json.createArrayBuilder().add(expandedIndex);

                            final JsonValue existingType = item.asJsonObject().get(Keywords.TYPE);

                            if (JsonUtils.isNotNull(existingType)) {

                                if (JsonUtils.isArray(existingType)) {
                                    existingType.asJsonArray().forEach(types::add);

                                } else {
                                    types.add(existingType);
                                }
                            }

                            item = Json.createObjectBuilder(item.asJsonObject()).add(Keywords.TYPE, types).build();
                        }

                        // 13.8.3.7.6.
                        expandedValue = Json.createArrayBuilder(expandedValue.asJsonArray()).add(item).build();
                    }
                }

            // 13.9.
            } else {

                expandedValue = Expansion
                                    .with(activeContext, value, key, baseUrl)
                                    .frameExpansion(frameExpansion)
                                    .ordered(ordered)
                                    .compute();
            }

            // 13.10.
            if (JsonUtils.isNull(expandedValue)) {
                continue;
            }

            // 13.11.
            if (containerMapping.contains(Keywords.LIST) && !ListObject.isListObject(expandedValue)) {
                expandedValue = ListObject.toListObject(expandedValue);
            }

            // 13.12.
            if (containerMapping.contains(Keywords.GRAPH)
                    && !containerMapping.contains(Keywords.ID)
                    && !containerMapping.contains(Keywords.INDEX)) {

                final JsonArrayBuilder array = Json.createArrayBuilder();

                JsonUtils
                    .toStream(expandedValue)
                    .map(GraphObject::toGraphObject)
                    .forEach(array::add);

                expandedValue = array.build();
            }

            // 13.13.
            if (keyTermDefinition.filter(TermDefinition::isReverseProperty).isPresent()) {

                // 13.13.3.
                expandedValue = JsonUtils.toJsonArray(expandedValue);

                // 13.13.4.
                for (JsonValue item : expandedValue.asJsonArray()) {

                    // 13.13.4.1.
                    if (ListObject.isListObject(item) || ValueObject.isValueObject(item)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY_VALUE);
                    }

                    // 13.13.4.3.
                    result.getMapBuilder(Keywords.REVERSE).add(expandedProperty, item);
                }


            // 13.14
            } else {
                result.add(expandedProperty, expandedValue);
            }
        }

        // 14.
        if (nest != null) {
            processNest();
        }
    }

    private void recurse() throws JsonLdError {

        // step 3
        final JsonValue propertyContext = activeContext
                .getTerm(activeProperty)
                .map(TermDefinition::getLocalContext)
                .orElse(null);


        // step 8
        if (propertyContext != null) {

            activeContext = activeContext
                                .newContext()
                                .overrideProtected(true)
                                .create(
                                    propertyContext,
                                    activeContext
                                            .getTerm(activeProperty)
                                            .map(TermDefinition::getBaseUrl)
                                            .orElse(null)
                                        );
        }

        // steps 13-14
        expand();
    }

    private final void processNest() throws JsonLdError {

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
