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
package no.hasmac.jsonld.compaction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonMapBuilder;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.GraphObject;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.ListObject;
import no.hasmac.jsonld.lang.NodeObject;
import no.hasmac.jsonld.lang.Utils;

import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#compaction-algorithm">Compaction Algorithm</a>
 *
 */
public final class Compaction {

    // required
    private final ActiveContext context;

    // optional
    private boolean compactArrays;
    private boolean ordered;

    private Compaction(final ActiveContext context) {
        this.context = context;

        // default values
        this.compactArrays = false;
        this.ordered = false;
    }

    public static Compaction with(final ActiveContext activeContext) {
        return new Compaction(activeContext);
    }

    public Compaction compactArrays(final boolean compactArrays) {
        this.compactArrays = compactArrays;
        return this;
    }

    public Compaction ordered(final boolean ordered) {
        this.ordered = ordered;
        return this;
    }

    public JsonValue compact(final JsonValue element) throws JsonLdError {
        return compact(null, element);
    }

    public JsonValue compact(final String activeProperty, final JsonValue element) throws JsonLdError {

        // 1.
        ActiveContext typeContext = context;
        ActiveContext activeContext = context;

        // 2.
        if (JsonUtils.isScalar(element)) {
            return element;
        }

        final Optional<TermDefinition> activePropertyDefinition = activeContext.getTerm(activeProperty);

        // 3.
        if (JsonUtils.isArray(element)) {

            // 3.1.
            final JsonArrayBuilder resultBuilder = JsonProvider.instance().createArrayBuilder();

            // 3.2.
            for (final JsonValue item : element.asJsonArray()) {

                // 3.2.1.
                final JsonValue compactedItem = Compaction
                                                    .with(activeContext)
                                                    .compactArrays(compactArrays)
                                                    .ordered(ordered)
                                                    .compact(activeProperty, item);
                // 3.2.2.
                if (JsonUtils.isNotNull(compactedItem)) {
                    resultBuilder.add(compactedItem);
                }
            }

            final JsonArray result = resultBuilder.build();

            // 3.3.
            if (result.isEmpty()
                    || result.size() > 1
                    || !compactArrays
                    || Keywords.GRAPH.equals(activeProperty)
                    || Keywords.SET.equals(activeProperty)
                    || activePropertyDefinition
                            .filter(d -> d.hasContainerMapping(Keywords.LIST) || d.hasContainerMapping(Keywords.SET))
                            .isPresent()
                    ) {

                return result;
            }

            // 3.4.
            return result.get(0);
        }

        // 4.
        final JsonObject elementObject = element.asJsonObject();

        // 5.
        if (activeContext.getPreviousContext() != null
                && !elementObject.containsKey(Keywords.VALUE)
                && !(elementObject.containsKey(Keywords.ID)
                        && elementObject.size() == 1)
                ) {
            activeContext = activeContext.getPreviousContext();
        }

        // 6.
        if (activePropertyDefinition.map(TermDefinition::getLocalContext).isPresent()) {

            activeContext =
                    activeContext
                            .newContext()
                            .overrideProtected(true)
                            .create(activePropertyDefinition.get().getLocalContext(),
                                    activePropertyDefinition.get().getBaseUrl());
        }

        // 7.
        if ((elementObject.containsKey(Keywords.VALUE)
                || elementObject.containsKey(Keywords.ID))
                && (!activeContext.getOptions().isRdfStar() || !elementObject.containsKey(Keywords.ANNOTATION))
                ) {

            final JsonValue result = activeContext.valueCompaction().compact(elementObject, activeProperty);

            if (JsonUtils.isScalar(result)
                    || activePropertyDefinition
                            .map(TermDefinition::getTypeMapping)
                            .filter(Keywords.JSON::equals)
                            .isPresent()
                    ) {

                return result;
            }

        }

        // 8.
        if (ListObject.isListObject(element)
                && activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.LIST)).isPresent()
                ) {

            return Compaction
                            .with(activeContext)
                            .compactArrays(compactArrays)
                            .ordered(ordered)
                            .compact(activeProperty, elementObject.get(Keywords.LIST));
        }

        // 9.
        final boolean insideReverse = Keywords.REVERSE.equals(activeProperty);

        // 10.
        final JsonMapBuilder result = JsonMapBuilder.create();

        // 11.
        if (elementObject.containsKey(Keywords.TYPE)) {

            final List<String> compactedTypes = new ArrayList<>();

            for (final JsonValue type : JsonUtils.toCollection(elementObject.get(Keywords.TYPE))) {
                compactedTypes.add(activeContext.uriCompaction().vocab(true).compact(((JsonString)type).getString()));
            }

            Collections.sort(compactedTypes);

            for (final String term : compactedTypes) {

                final Optional<TermDefinition> termDefinition = typeContext.getTerm(term);

                // 11.1.
                if (termDefinition.filter(TermDefinition::hasLocalContext).isPresent()) {

                    activeContext =
                            activeContext
                                .newContext()
                                .propagate(false)
                                .create(termDefinition.get().getLocalContext(), termDefinition.get().getBaseUrl());
                }
            }
        }

        // 12.
        for (final String expandedProperty : Utils.index(elementObject.keySet(), ordered)) {

            final JsonValue expandedValue = elementObject.get(expandedProperty);

            // 12.1.
            if (Keywords.ID.equals(expandedProperty)) {

                JsonValue compactedValue = JsonValue.NULL;

                // 12.1.1.
                if (JsonUtils.isString(expandedValue)) {
                    compactedValue = JsonUtils.toJsonValue(activeContext.uriCompaction().compact(((JsonString)expandedValue).getString()));

                // json-ld-star
                } else if (activeContext.getOptions().isRdfStar() && NodeObject.isEmbeddedNode(expandedValue)) {
                    compactedValue = Compaction.with(activeContext)
                                        .compactArrays(compactArrays)
                                        .ordered(ordered)
                                        .compact(expandedValue);
                }

                // 12.1.2.
                final String alias = activeContext.uriCompaction().vocab(true).compact(expandedProperty);

                // 12.1.3.
                result.put(alias, compactedValue);

                continue;
            }

            // 12.2.
            if (Keywords.TYPE.equals(expandedProperty)) {

                final JsonValue compactedValue;

                // 12.2.1.
                if (JsonUtils.isString(expandedValue)) {
                    compactedValue = JsonUtils.toJsonValue(typeContext.uriCompaction().vocab(true).compact(((JsonString)expandedValue).getString()));

                // 12.2.2.
                } else if (JsonUtils.isArray(expandedValue)) {

                    // 12.2.2.1.
                    final JsonArrayBuilder compactedArray = JsonProvider.instance().createArrayBuilder();

                    // 12.2.2.2.
                    for (final JsonValue expandedType : expandedValue.asJsonArray()) {

                        compactedArray.add(
                                        typeContext
                                            .uriCompaction()
                                            .vocab(true)
                                            .compact(((JsonString)expandedType).getString())
                                        );
                    }

                    compactedValue = compactedArray.build();

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_VALUE, "@type value is not valid [" + expandedValue + "].");
                }

                // 12.2.3.
                final String alias = activeContext.uriCompaction().vocab(true).compact(expandedProperty);

                // 12.2.4.
                final boolean asArray = !compactArrays
                                        || (activeContext.inMode(JsonLdVersion.V1_1)
                                            && activeContext.getTerm(alias).filter(t -> t.hasContainerMapping(Keywords.SET)).isPresent()
                                           );

                // 12.2.5.
                result.add(alias, compactedValue, asArray);

                // 12.2.6.
                continue;
            }

            // 12.3.
            if (Keywords.REVERSE.equals(expandedProperty)) {

                // 12.3.1.
                final JsonObject compactedMap = Compaction
                                                    .with(activeContext)
                                                    .compactArrays(compactArrays)
                                                    .ordered(ordered)
                                                    .compact(Keywords.REVERSE, expandedValue)
                                                    .asJsonObject();

                JsonObjectBuilder remaining = null;

                // 12.3.2.
                for (final Entry<String, JsonValue> entry : compactedMap.entrySet()) {

                    // 12.3.2.1.
                    if (activeContext.getTerm(entry.getKey()).filter(TermDefinition::isReverseProperty).isPresent()) {

                        // 12.3.2.1.1
                        final boolean asArray = !compactArrays
                                                    || activeContext
                                                                .getTerm(entry.getKey())
                                                                .filter(td -> td.hasContainerMapping(Keywords.SET))
                                                                .isPresent();

                        // 12.3.2.1.2.
                        result.add(entry.getKey(), entry.getValue(), asArray);

                    } else {

                        if (remaining == null) {
                            remaining = JsonProvider.instance().createObjectBuilder();
                        }

                        remaining.add(entry.getKey(), entry.getValue());
                    }

                }

                // 12.8.3.
                if (remaining != null) {

                    // 12.8.3.1.
                    final String alias = activeContext.uriCompaction().vocab(true).compact(Keywords.REVERSE);

                    // 12.8.3.2.
                    result.put(alias, remaining.build());
                }

                // 12.8.4.
                continue;
            }

            // 12.4.
            if (Keywords.PRESERVE.equals(expandedProperty)) {

                // 12.4.1.
                final JsonValue compactedValue = Compaction
                                                    .with(activeContext)
                                                    .compactArrays(compactArrays)
                                                    .ordered(ordered)
                                                    .compact(activeProperty, expandedValue);

                // 12.4.2.
                if (!JsonUtils.isEmptyArray(compactedValue)) {
                    result.put(Keywords.PRESERVE, compactedValue);
                }
                continue;
            }

            // 12.5.
            if (Keywords.INDEX.equals(expandedProperty)
                    && activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.INDEX)).isPresent()
                    ) {
                continue;

            // 12.6.
            } else if (Keywords.anyMatch(expandedProperty,
                                            Keywords.DIRECTION,
                                            Keywords.INDEX,
                                            Keywords.LANGUAGE,
                                            Keywords.VALUE
                                            )) {
                // 12.6.1.
                final String alias = activeContext.uriCompaction().vocab(true).compact(expandedProperty);

                // 12.6.2.
                result.put(alias, expandedValue);

                continue;
            }

            // 12.7.
            if (JsonUtils.isEmptyArray(expandedValue)) {

                // 12.7.1.
                final String itemActiveProperty = activeContext
                                                    .uriCompaction()
                                                    .value(expandedValue)
                                                    .vocab(true)
                                                    .reverse(insideReverse)
                                                    .compact(expandedProperty);
                // 12.7.2.
                final Optional<String> nestProperty = activeContext
                                                        .getTerm(itemActiveProperty)
                                                        .map(TermDefinition::getNestValue);

                if (nestProperty.isPresent()) {

                    final String nestTerm = nestProperty.get();

                    // 12.7.2.1.
                    if (!Keywords.NEST.equals(nestTerm) && !Keywords.NEST.equals(activeContext.uriExpansion().vocab(true).expand(nestTerm))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }

                    // 12.7.2.3.
                    result.getMapBuilder(nestTerm).add(itemActiveProperty, JsonValue.EMPTY_JSON_ARRAY);


                // 12.7.3.
                } else {
                    result.add(itemActiveProperty, JsonValue.EMPTY_JSON_ARRAY);
                }

            }

            // 12.8.
            for (final JsonValue expandedItem : expandedValue.asJsonArray()) {

                // 12.8.1.
                final String itemActiveProperty = activeContext
                                                .uriCompaction()
                                                .value(expandedItem)
                                                .vocab(true)
                                                .reverse(insideReverse)
                                                .compact(expandedProperty);

                JsonMapBuilder nestResult = null;
                String nestResultKey = null;

                // 12.8.2.
                final Optional<String> nestProperty = activeContext
                                                        .getTerm(itemActiveProperty)
                                                        .map(TermDefinition::getNestValue);

                if (nestProperty.isPresent()) {

                    final String nestTerm = nestProperty.get();

                    // 12.8.2.1.
                    if (!Keywords.NEST.equals(nestTerm) && !Keywords.NEST.equals(activeContext.uriExpansion().vocab(true).expand(nestTerm))) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
                    }

                    // 12.8.2.3.
                    nestResult = result.getMapBuilder(nestTerm);
                    nestResultKey = nestTerm;

                // 12.8.3.
                } else {
                    nestResult = result;
                }

                // 12.8.4.
                final Collection<String> container = activeContext
                                                        .getTerm(itemActiveProperty)
                                                        .map(TermDefinition::getContainerMapping)
                                                        .orElseGet(() -> Collections.emptyList());

                // 12.8.5.
                final boolean asArray = container.contains(Keywords.SET)
                                        || Keywords.GRAPH.equals(itemActiveProperty)
                                        || Keywords.LIST.equals(itemActiveProperty)
                                        || !compactArrays;

                // 12.8.6.
                JsonValue expandedItemValue = expandedItem;

                if (ListObject.isListObject(expandedItem)) {
                    expandedItemValue = expandedItem.asJsonObject().get(Keywords.LIST);

                } else if (GraphObject.isGraphObject(expandedItem)) {
                    expandedItemValue = expandedItem.asJsonObject().get(Keywords.GRAPH);
                }

                JsonValue compactedItem = Compaction
                                                .with(activeContext)
                                                .compactArrays(compactArrays)
                                                .ordered(ordered)
                                                .compact(itemActiveProperty, expandedItemValue);

                // 12.8.7.
                if (ListObject.isListObject(expandedItem)) {

                    // 12.8.7.1.
                    compactedItem = JsonUtils.toJsonArray(compactedItem);

                    // 12.8.7.2.
                    if (!container.contains(Keywords.LIST)) {

                        // 12.8.7.2.1.
                        String key = activeContext.uriCompaction().vocab(true).compact(Keywords.LIST);

                        compactedItem = JsonProvider.instance().createObjectBuilder().add(key, compactedItem).build();

                        // 12.8.7.2.2.
                        if (JsonUtils.containsKey(expandedItem, Keywords.INDEX)) {

                            String indexKey = activeContext.uriCompaction().vocab(true).compact(Keywords.INDEX);

                            compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject())
                                                .add(indexKey, expandedItem.asJsonObject().get(Keywords.INDEX))
                                                .build();
                        }

                        // 12.8.7.2.3.
                        nestResult.add(itemActiveProperty, compactedItem, asArray);

                    // 12.8.7.3.
                    } else {
                        nestResult.put(itemActiveProperty, compactedItem);
                    }

                // 12.8.8.
                } else if (GraphObject.isGraphObject(expandedItem)) {

                    boolean followup = false;

                    // 12.8.8.1.
                    if (container.contains(Keywords.GRAPH) && container.contains(Keywords.ID)) {

                        // 12.8.8.1.2.
                        String mapKey = null;

                        if (expandedItem.asJsonObject().containsKey(Keywords.ID)) {
                            String id = expandedItem.asJsonObject().getString(Keywords.ID);
                            mapKey = activeContext.uriCompaction().compact(id);

                        } else {
                            mapKey = activeContext.uriCompaction().vocab(true).compact(Keywords.NONE);
                        }

                        // 12.8.8.1.3.
                        nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);

                    // 12.8.8.2.
                    } else if (container.contains(Keywords.GRAPH)
                                    && container.contains(Keywords.INDEX)
                                    && GraphObject.isSimpleGraphObject(expandedItem)
                                            ) {

                        // 12.8.8.2.2.
                        final String mapKey = expandedItem.asJsonObject().containsKey(Keywords.INDEX)
                                            ? expandedItem.asJsonObject().getString(Keywords.INDEX)
                                            : Keywords.NONE;

                        // 12.8.8.2.3.
                        nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);

                    // 12.8.8.3.
                    } else if (container.contains(Keywords.GRAPH)
                            && GraphObject.isSimpleGraphObject(expandedItem)
                                    ) {

                        // 12.8.8.3.1.
                        if (JsonUtils.isArray(compactedItem) && compactedItem.asJsonArray().size() > 1) {
                            compactedItem = JsonProvider.instance().createObjectBuilder().add(
                                                        activeContext
                                                            .uriCompaction()
                                                            .vocab(true)
                                                            .compact(Keywords.INCLUDED),
                                                        compactedItem
                                                    ).build();
                        }

                        // 12.8.8.3.2.
                        nestResult.add(itemActiveProperty, compactedItem, asArray);

                    } else {
                        followup = true;
                    }

                    // 12.8.8.4.
                    if (!container.contains(Keywords.GRAPH) || followup) {
                        // 12.8.8.4.1.
                        compactedItem = JsonProvider.instance().createObjectBuilder().add(
                                activeContext
                                    .uriCompaction()
                                    .vocab(true)
                                    .compact(Keywords.GRAPH),
                                compactedItem
                            ).build();

                        // 12.8.8.4.2.
                        if (expandedItem.asJsonObject().containsKey(Keywords.ID)) {

                            compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).add(
                                    activeContext
                                        .uriCompaction()
                                        .vocab(true)
                                        .compact(Keywords.ID),

                                    activeContext
                                        .uriCompaction()
                                        .compact(expandedItem.asJsonObject().getString(Keywords.ID))

                                ).build();
                        }

                        // 12.8.8.4.3.
                        if (expandedItem.asJsonObject().containsKey(Keywords.INDEX)) {

                            compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).add(
                                    activeContext
                                        .uriCompaction()
                                        .vocab(true)
                                        .compact(Keywords.INDEX),
                                    expandedItem.asJsonObject().getString(Keywords.INDEX)
                                ).build();
                        }

                        // 12.8.8.4.4.
                        nestResult.add(itemActiveProperty, compactedItem, asArray);
                    }

                // 12.8.9.
                } else if ((container.contains(Keywords.LANGUAGE)
                            || container.contains(Keywords.INDEX)
                            || container.contains(Keywords.ID)
                            || container.contains(Keywords.TYPE))
                            && !container.contains(Keywords.GRAPH)
                        ) {

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

                    String containerKey = activeContext.uriCompaction().vocab(true).compact(keyToCompact);

                    // 12.8.9.3.
                    final String indexKey = activeContext
                                                .getTerm(itemActiveProperty)
                                                .map(TermDefinition::getIndexMapping)
                                                .orElse(Keywords.INDEX);

                    String mapKey = null;

                    // 12.8.9.4.
                    if (container.contains(Keywords.LANGUAGE)
                            && expandedItem.asJsonObject().containsKey(Keywords.VALUE)
                            ) {

                        if (JsonUtils.isObject(compactedItem)) {
                            compactedItem = compactedItem.asJsonObject().get(Keywords.VALUE);
                        }

                        if (expandedItem.asJsonObject().containsKey(Keywords.LANGUAGE)) {

                            mapKey = expandedItem.asJsonObject().getString(Keywords.LANGUAGE);
                        }

                    // 12.8.9.5.
                    } else if (container.contains(Keywords.INDEX)
                                && Keywords.INDEX.equals(indexKey)) {

                        if (expandedItem.asJsonObject().containsKey(Keywords.INDEX)) {

                            mapKey = expandedItem.asJsonObject().getString(Keywords.INDEX);
                        }

                    // 12.8.9.6.
                    } else if (container.contains(Keywords.INDEX)
                                && !Keywords.INDEX.equals(indexKey)) {

                        // 12.8.9.6.1.
                        containerKey = activeContext
                                                .uriCompaction()
                                                .vocab(true)
                                                .compact(activeContext.uriExpansion().expand(indexKey));

                        // 12.8.9.6.2.
                        if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            final JsonValue containerValue = compactedItem.asJsonObject().get(containerKey);

                            if (JsonUtils.isString(containerValue)) {
                                mapKey = ((JsonString)containerValue).getString();

                                // 12.8.9.6.3.
                                compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();

                            } else if (JsonUtils.isArray(containerValue) && !JsonUtils.isEmptyArray(containerValue)) {

                                mapKey = containerValue.asJsonArray().getString(0);

                                // 12.8.9.6.3.
                                if (containerValue.asJsonArray().size() > 1) {

                                    JsonValue containerKeyValue = null;

                                    if (containerValue.asJsonArray().size() == 2) {
                                        containerKeyValue = containerValue.asJsonArray().get(1);

                                    } else {
                                        containerKeyValue = JsonProvider.instance().createArrayBuilder(containerValue.asJsonArray()).remove(0).build();
                                    }

                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject())
                                                        .remove(containerKey)
                                                        .add(containerKey, containerKeyValue)
                                                        .build();

                                } else {
                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                                }
                            }
                        }

                    // 12.8.9.7.
                    } else if (container.contains(Keywords.ID)) {

                        if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            mapKey = compactedItem.asJsonObject().getString(containerKey);

                            compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                        }

                    // 12.8.9.8.
                    } else if (container.contains(Keywords.TYPE)) {

                        // 12.8.9.8.1.
                        if (JsonUtils.containsKey(compactedItem, containerKey)) {

                            final JsonValue compactedKeyValue = compactedItem.asJsonObject().get(containerKey);

                            if (JsonUtils.isNotNull(compactedKeyValue)) {

                                final JsonArray compactedKeyArray = JsonUtils.toJsonArray(compactedKeyValue);

                                mapKey = compactedKeyArray.getString(0);

                                if (compactedKeyArray.size() > 1) {

                                    JsonValue compactedKeyArrayValue = null;
                                    if (compactedKeyArray.size() == 2) {

                                        compactedKeyArrayValue = compactedKeyArray.get(1);

                                    } else {
                                        compactedKeyArrayValue = JsonProvider.instance().createArrayBuilder(compactedKeyArray).remove(0).build();
                                    }

                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject())
                                                        .remove(containerKey)
                                                        .add(containerKey, compactedKeyArrayValue)
                                                        .build();

                                } else {
                                    compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                                }


                            } else {
                                compactedItem = JsonProvider.instance().createObjectBuilder(compactedItem.asJsonObject()).remove(containerKey).build();
                            }
                        }

                        // 12.8.9.8.4.
                        if (JsonUtils.isObject(compactedItem) && compactedItem.asJsonObject().size() == 1) {

                            final String expandedKey = activeContext.uriExpansion().vocab(true).expand(compactedItem.asJsonObject().keySet().iterator().next());

                            if (Keywords.ID.equals(expandedKey)) {

                                JsonObject map = JsonProvider.instance().createObjectBuilder().add(Keywords.ID, expandedItem.asJsonObject().get(Keywords.ID)).build();

                                compactedItem = Compaction
                                                        .with(activeContext)
                                                        .compact(itemActiveProperty, map);
                            }
                        }
                    }

                    // 12.8.9.9.
                    if (mapKey == null) {
                        mapKey = activeContext.uriCompaction().vocab(true).compact(Keywords.NONE);
                    }

                    // 12.8.9.10.
                    nestResult.getMapBuilder(itemActiveProperty).add(mapKey, compactedItem, asArray);

                // 12.8.10.
                } else {
                    nestResult.add(itemActiveProperty, compactedItem, asArray);
                }

                if (nestResult != null && nestResultKey != null) {
                    result.put(nestResultKey, nestResult);
                }
            }
        }

        // 13.
        return result.build();
    }
}
