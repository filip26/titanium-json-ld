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
package no.hasmac.jsonld.expansion;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonMapBuilder;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.jsonld.lang.NodeObject;
import no.hasmac.jsonld.lang.Utils;
import no.hasmac.jsonld.uri.UriUtils;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;

/**
 * @see <a href=
 * "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 * Algorithm</a>
 */
public final class ObjectExpansion {

    // mandatory
    private ActiveContext activeContext;
    private JsonValue propertyContext;
    private JsonObject element;
    private String activeProperty;
    private URI baseUrl;

    // optional
    private boolean frameExpansion;
    private boolean ordered;
    private boolean fromMap;

    private ObjectExpansion(final ActiveContext activeContext, final JsonValue propertyContext, final JsonObject element,
                            final String activeProperty, final URI baseUrl) {
        this.activeContext = activeContext;
        this.propertyContext = propertyContext;
        this.element = element;
        this.activeProperty = activeProperty;
        this.baseUrl = baseUrl;

        // default values
        this.frameExpansion = false;
        this.ordered = false;
        this.fromMap = false;
    }

    public static ObjectExpansion with(final ActiveContext activeContext, final JsonValue propertyContext,
                                       final JsonObject element, final String activeProperty, final URI baseUrl) {
        return new ObjectExpansion(activeContext, propertyContext, element, activeProperty, baseUrl);
    }

    public ObjectExpansion frameExpansion(boolean value) {
        this.frameExpansion = value;
        return this;
    }

    public ObjectExpansion ordered(boolean value) {
        this.ordered = value;
        return this;
    }

    public ObjectExpansion fromMap(boolean value) {
        this.fromMap = value;
        return this;
    }

    public JsonValue expand() throws JsonLdError {

        initPreviousContext();

        initPropertyContext();

        initLocalContext();

        // 10.
        final ActiveContext typeContext = activeContext;

        final String typeKey = processTypeScoped(typeContext);

        final String inputType = findInputType(typeKey);

        final JsonMapBuilder result = JsonMapBuilder.create();

        ObjectExpansion1314
                .with(activeContext, element, activeProperty, baseUrl)
                .inputType(inputType)
                .result(result)
                .typeContext(typeContext)
                .nest(new LinkedHashMap<>())
                .frameExpansion(frameExpansion)
                .ordered(ordered)
                .expand();

        // 15.
        if (result.containsKey(Keywords.VALUE)) {

            return normalizeValue(result);

            // 16.
        } else if (result.containsKey(Keywords.TYPE)) {

            return normalizeType(result);

            // 17.
        } else if (result.containsKey(Keywords.LIST) || result.containsKey(Keywords.SET)) {

            return normalizeContainer(result);
        }

        return normalize(result);
    }

    private void initPropertyContext() throws JsonLdError {
        // 8.
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
    }

    private void initPreviousContext() throws JsonLdError {

        // 7. If active context has a previous context, the active context is not
        // propagated.
        // If from map is undefined or false, and element does not contain an entry
        // expanding to @value,
        // and element does not consist of a single entry expanding to @id (where
        // entries are IRI expanded),
        // set active context to previous context from active context,
        // as the scope of a term-scoped context does not apply when processing new node
        // objects.
        if (activeContext.getPreviousContext() != null && !fromMap) {

            boolean revert = true;

            for (final String key : Utils.index(element.keySet(), true)) {

                final String expandedKey =
                        activeContext
                                .uriExpansion()
                                .vocab(true)
                                .expand(key);

                if (Keywords.VALUE.equals(expandedKey) || (Keywords.ID.equals(expandedKey) && (element.size() == 1))) {
                    revert = false;
                    break;
                }
            }

            if (revert) {
                activeContext = activeContext.getPreviousContext();
            }
        }
    }

    private void initLocalContext() throws JsonLdError {
        // 9.
        if (element.containsKey(Keywords.CONTEXT)) {

            activeContext = activeContext
                    .newContext()
                    .create(element.get(Keywords.CONTEXT), baseUrl);
        }
    }

    private String processTypeScoped(final ActiveContext typeContext) throws JsonLdError {

        if (element.containsKey(Keywords.TYPE)) {
            // TODO: This is safe only if an element can't have two different keys for TYPE.
            // 11.2

            JsonValue value = element.get(Keywords.TYPE);
            List<String> terms = JsonUtils.optimizedGetStrings(value);

            for (final String term : terms) {

                Optional<JsonValue> localContext = typeContext.getTerm(term).map(TermDefinition::getLocalContext);

                if (localContext.isPresent()) {

                    Optional<TermDefinition> valueDefinition = activeContext.getTerm(term);

                    activeContext =
                            activeContext
                                    .newContext()
                                    .propagate(false)
                                    .create(localContext.get(),
                                            valueDefinition
                                                    .map(TermDefinition::getBaseUrl)
                                                    .orElse(null)
                                    );
                }
            }
            return Keywords.TYPE;
        }

        String typeKey = null;

        // 11.
        for (final String key : Utils.index(element.keySet(), true)) {

            final String expandedKey =
                    activeContext
                            .uriExpansion()
                            .vocab(true)
                            .expand(key);

            if (!Keywords.TYPE.equals(expandedKey)) {
                continue;

            } else if (typeKey == null) {
                typeKey = key;
            }

            // 11.2

            JsonValue value = element.get(key);
            List<String> terms = JsonUtils.optimizedGetStrings(value);

            for (final String term : terms) {

                Optional<JsonValue> localContext = typeContext.getTerm(term).map(TermDefinition::getLocalContext);

                if (localContext.isPresent()) {

                    Optional<TermDefinition> valueDefinition = activeContext.getTerm(term);

                    activeContext =
                            activeContext
                                    .newContext()
                                    .propagate(false)
                                    .create(localContext.get(),
                                            valueDefinition
                                                    .map(TermDefinition::getBaseUrl)
                                                    .orElse(null)
                                    );
                }
            }
        }

        return typeKey;
    }


    private String findInputType(final String typeKey) throws JsonLdError {

        // Initialize input type to expansion of the last value of the first entry in
        // element
        // expanding to @type (if any), ordering entries lexicographically by key. Both
        // the key and
        // value of the matched entry are IRI expanded.
        if (typeKey != null) {

            final JsonValue type = element.get(typeKey);

            if (JsonUtils.isArray(type)) {

                final String lastValue = type.asJsonArray()
                        .stream()
                        .filter(JsonUtils::isString)
                        .map(JsonString.class::cast)
                        .map(JsonString::getString)
                        .sorted()
                        .reduce((first, second) -> second)
                        .orElse(null);

                if (lastValue != null) {
                    return activeContext.uriExpansion().vocab(true).expand(lastValue);
                }

            } else if (JsonUtils.isString(type)) {
                return activeContext.uriExpansion().vocab(true).expand(((JsonString) type).getString());
            }
        }

        return null;
    }

    private JsonValue normalizeValue(final JsonMapBuilder result) throws JsonLdError {

        // 15.1.
        if (result.isNotValueObject()) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
        }

        if ((result.containsKey(Keywords.DIRECTION) || result.containsKey(Keywords.LANGUAGE))
                && result.containsKey(Keywords.TYPE)) {

            throw new JsonLdError(JsonLdErrorCode.INVALID_VALUE_OBJECT);
        }

        // 15.2.
        final Optional<JsonValue> type = result.get(Keywords.TYPE);

        if (type.map(t -> !JsonUtils.contains(Keywords.JSON, t)).orElse(true)) {

            final Optional<JsonValue> value = result.get(Keywords.VALUE);

            // 15.3.
            if (value.map(v -> JsonUtils.isNull(v) || (JsonUtils.isArray(v) && v.asJsonArray().isEmpty())).orElse(true)) {
                return JsonValue.NULL;

                // 15.4
            } else if (!frameExpansion && JsonUtils.isNotString(value.get()) && result.containsKey(Keywords.LANGUAGE)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_VALUE);

                // 15.5
            } else if (!frameExpansion
                    && type.filter(t -> JsonUtils.isNotString(t)
                            || UriUtils.isNotURI(((JsonString) t).getString()))
                    .isPresent()
            ) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPED_VALUE);
            }
        }

        return normalize(result);
    }

    private JsonValue normalizeType(final JsonMapBuilder result) throws JsonLdError {

        result
                .get(Keywords.TYPE)
                .filter(JsonUtils::isNotArray)
                .filter(JsonUtils::isNotNull)
                .ifPresent(value -> result.put(Keywords.TYPE, JsonProvider.instance().createArrayBuilder().add(value).build()));

        return normalize(result);
    }

    private JsonValue normalizeContainer(final JsonMapBuilder result) throws JsonLdError {

        // 17.1.
        if (result.size() > 2 || result.size() == 2 && !result.containsKey(Keywords.INDEX)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_SET_OR_LIST_OBJECT);
        }

        // 17.2.
        final Optional<JsonValue> set = result.get(Keywords.SET);

        if (set.filter(JsonUtils::isObject).isPresent()) {
            // deepcode ignore checkIsPresent~Optional: false positive
            return normalize(JsonMapBuilder.create(set.map(JsonValue::asJsonObject).get()));

        } else if (set.isPresent()) {
            return set.get();
        }

        return normalize(result);
    }

    private JsonValue normalize(final JsonMapBuilder result) throws JsonLdError {

        // Extension: JSON-LD-STAR (Experimental)
        if (result.containsKey(Keywords.ANNOTATION)
                && (StringUtils.isBlank(activeProperty)
                || Keywords.GRAPH.equals(activeProperty)
                || Keywords.INCLUDED.equals(activeProperty)
                || result.get(Keywords.ANNOTATION).filter(NodeObject::isNotAnnotationObject).isPresent())
        ) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_ANNOTATION);
        }

        // 18.
        if (result.size() == 1 && result.containsKey(Keywords.LANGUAGE)) {
            return JsonValue.NULL;
        }

        // 19.
        if (activeProperty == null || Keywords.GRAPH.equals(activeProperty)) {

            // 19.1. If result is a map which is empty, or contains only the entries @value
            // or @list, set result to null
            if (!frameExpansion && result.isEmpty()
                    || result.containsKey(Keywords.VALUE)
                    || result.containsKey(Keywords.LIST)) {
                return JsonValue.NULL;
            }

            // 19.2. if result is a map whose only entry is @id, set result to null. When
            // the frameExpansion flag is set, a map containing only the @id entry is
            // retained.
            if (!frameExpansion && result.size() == 1 && result.containsKey(Keywords.ID)) {
                return JsonValue.NULL;
            }

        }

        return result.build();
    }
}
