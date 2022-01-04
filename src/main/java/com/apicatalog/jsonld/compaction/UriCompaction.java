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

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.context.InverseContext;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.GraphObject;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriRelativizer;

import jakarta.json.JsonArray;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#iri-compaction">IRI Compaction</a>
 *
 */
public final class UriCompaction {

    // required
    private final ActiveContext activeContext;

    // optional
    private JsonValue value;
    private boolean vocab;
    private boolean reverse;

    private UriCompaction(final ActiveContext activeContext) {
        this.activeContext = activeContext;

        // default values
        this.value = null;
        this.vocab = false;
        this.reverse = false;
    }

    public static UriCompaction with(final ActiveContext activeContext) {
        return new UriCompaction(activeContext);
    }

    public UriCompaction value(JsonValue value) {
        this.value = value;
        return this;
    }

    public UriCompaction vocab(boolean vocab) {
        this.vocab = vocab;
        return this;
    }

    public UriCompaction reverse(boolean reverse) {
        this.reverse = reverse;
        return this;
    }

    public String compact(final String variable) throws JsonLdError {

        // 1.
        if (variable == null) {
            return null;
        }

        // 2.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        // 3.
        InverseContext inverseContext = activeContext.getInverseContext();

        // 4.
        if (vocab && inverseContext.contains(variable)) {

            // 4.1.
            String defaultLanguage = Keywords.NONE;

            if (activeContext.getDefaultLanguage() != null) {

                defaultLanguage = activeContext.getDefaultLanguage().toLowerCase();

                if (activeContext.getDefaultBaseDirection() != null) {
                    defaultLanguage += "_".concat(activeContext.getDefaultBaseDirection().name().toLowerCase());
                }

            } else if (activeContext.getDefaultBaseDirection() != null) {
                defaultLanguage = "_".concat(activeContext.getDefaultBaseDirection().name().toLowerCase());
            }

            // 4.2.
            if (JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.PRESERVE)) {

                JsonValue preserve = value.asJsonObject().get(Keywords.PRESERVE);

                if (JsonUtils.isNotNull(preserve)) {
                    value = JsonUtils.toJsonArray(preserve).get(0);
                }
            }

            // 4.3.
            Collection<String> containers = new ArrayList<>();

            // 4.4.
            String typeLanguage = Keywords.LANGUAGE;
            String typeLanguageValue = Keywords.NULL;

            // 4.5.
            if (JsonUtils.isObject(value)
                    && value.asJsonObject().containsKey(Keywords.INDEX)
                    && !GraphObject.isGraphObject(value)
                    ) {

                containers.add(Keywords.INDEX);
                containers.add(Keywords.INDEX.concat(Keywords.SET));
            }

            // 4.6.
            if (reverse) {

                typeLanguage = Keywords.TYPE;
                typeLanguageValue = Keywords.REVERSE;

                containers.add(Keywords.SET);

            // 4.7.
            } else if (ListObject.isListObject(value)) {

                // 4.7.1.
                if (!value.asJsonObject().containsKey(Keywords.INDEX)) {
                    containers.add(Keywords.LIST);
                }

                // 4.7.2.
                JsonArray list = value.asJsonObject().get(Keywords.LIST).asJsonArray();

                // 4.7.3.
                String commonType = null;
                String commonLanguage = list.isEmpty()
                                            ? defaultLanguage
                                            : null;
                // 4.7.4.
                for (JsonValue item : list) {

                    // 4.7.4.1.
                    String itemLanguage = Keywords.NONE;
                    String itemType = Keywords.NONE;

                    // 4.7.4.2.
                    if (JsonUtils.isObject(item)
                            && item.asJsonObject().containsKey(Keywords.VALUE)) {

                        // 4.7.4.2.1.
                        if (item.asJsonObject().containsKey(Keywords.DIRECTION)) {

                            itemLanguage = "";

                            if (item.asJsonObject().containsKey(Keywords.LANGUAGE)) {
                                itemLanguage = item.asJsonObject().getString(Keywords.LANGUAGE).toLowerCase();
                            }

                            itemLanguage += "_".concat(item.asJsonObject().getString(Keywords.DIRECTION).toLowerCase());

                        // 4.7.4.2.2.
                        } else if (item.asJsonObject().containsKey(Keywords.LANGUAGE)) {

                            itemLanguage = item.asJsonObject().getString(Keywords.LANGUAGE).toLowerCase();

                        // 4.7.4.2.3.
                        } else if (item.asJsonObject().containsKey(Keywords.TYPE)) {

                            itemType = item.asJsonObject().getString(Keywords.TYPE);

                        // 4.7.4.2.4.
                        } else {
                            itemLanguage = Keywords.NULL;
                        }

                    // 4.7.4.3.
                    } else {
                        itemType = Keywords.ID;
                    }

                    // 4.7.4.4.
                    if (commonLanguage == null) {
                        commonLanguage = itemLanguage;

                    // 4.7.4.5.
                    } else if (!Objects.equals(itemLanguage, commonLanguage)
                            && JsonUtils.isObject(item)
                            && item.asJsonObject().containsKey(Keywords.VALUE)
                            ) {
                        commonLanguage = Keywords.NONE;
                    }

                    // 4.7.4.6.
                    if (commonType == null) {
                        commonType = itemType;

                    // 4.7.4.7.
                    } else if (!Objects.equals(itemType, commonType)) {
                        commonType = Keywords.NONE;
                    }

                    // 4.7.4.8.
                    if (Keywords.NONE.equals(commonLanguage) && Keywords.NONE.equals(commonType)) {
                        break;
                    }
                }

                // 4.7.5.
                if (commonLanguage == null) {
                    commonLanguage = Keywords.NONE;
                }

                // 4.7.6.
                if (commonType == null) {
                    commonType = Keywords.NONE;
                }

                // 4.7.7.
                if (!Keywords.NONE.equals(commonType)) {
                    typeLanguage = Keywords.TYPE;
                    typeLanguageValue = commonType;

                // 4.7.8.
                } else {
                    typeLanguageValue = commonLanguage;
                }

            // 4.8.
            } else if (GraphObject.isGraphObject(value)) {

                // 4.8.1.
                if (value.asJsonObject().containsKey(Keywords.INDEX)) {
                    containers.add(Keywords.GRAPH.concat(Keywords.INDEX));
                    containers.add(Keywords.GRAPH.concat(Keywords.INDEX).concat(Keywords.SET));
                }

                // 4.8.2.
                if (value.asJsonObject().containsKey(Keywords.ID)) {
                    containers.add(Keywords.GRAPH.concat(Keywords.ID));
                    containers.add(Keywords.GRAPH.concat(Keywords.ID).concat(Keywords.SET));
                }

                // 4.8.3.
                containers.add(Keywords.GRAPH);
                containers.add(Keywords.GRAPH.concat(Keywords.SET));
                containers.add(Keywords.SET);

                // 4.8.4.
                if (!value.asJsonObject().containsKey(Keywords.INDEX)) {
                    containers.add(Keywords.GRAPH.concat(Keywords.INDEX));
                    containers.add(Keywords.GRAPH.concat(Keywords.INDEX).concat(Keywords.SET));
                }

                // 4.8.5.
                if (!value.asJsonObject().containsKey(Keywords.ID)) {
                    containers.add(Keywords.GRAPH.concat(Keywords.ID));
                    containers.add(Keywords.GRAPH.concat(Keywords.ID).concat(Keywords.SET));
                }

                // 4.8.6.
                containers.add(Keywords.INDEX);
                containers.add(Keywords.INDEX.concat(Keywords.SET));

                // 4.8.7.
                typeLanguage = Keywords.TYPE;
                typeLanguageValue = Keywords.ID;

            // 4.9.
            } else {

                // 4.9.1.
                if (ValueObject.isValueObject(value)) {

                    // 4.9.1.1.
                    if (JsonUtils.contains(Keywords.DIRECTION, value)
                            && !JsonUtils.contains(Keywords.INDEX, value)
                            ) {

                        typeLanguageValue = "";

                        if (JsonUtils.contains(Keywords.LANGUAGE, value)) {

                            JsonValue language = value.asJsonObject().get(Keywords.LANGUAGE);

                            if (JsonUtils.isString(language)) {
                                typeLanguageValue = ((JsonString)language).getString().toLowerCase();
                            }
                        }

                        JsonValue direction = value.asJsonObject().get(Keywords.DIRECTION);
                        if (JsonUtils.isString(direction)) {
                            typeLanguageValue += "_".concat(((JsonString)direction).getString().toLowerCase());
                        }

                        containers.add(Keywords.LANGUAGE);
                        containers.add(Keywords.LANGUAGE.concat(Keywords.SET));

                    // 4.9.1.2.
                    } else if (JsonUtils.contains(Keywords.LANGUAGE, value)
                            && !JsonUtils.contains(Keywords.INDEX, value)
                            ) {

                        if (JsonUtils.contains(Keywords.LANGUAGE, value)) {

                            JsonValue language = value.asJsonObject().get(Keywords.LANGUAGE);

                            if (JsonUtils.isString(language)) {
                                typeLanguageValue = ((JsonString)language).getString().toLowerCase();
                            }
                        }

                        containers.add(Keywords.LANGUAGE);
                        containers.add(Keywords.LANGUAGE.concat(Keywords.SET));

                    // 4.9.1.3.
                    } else if (JsonUtils.contains(Keywords.TYPE, value)) {

                        typeLanguage = Keywords.TYPE;
                        typeLanguageValue = value.asJsonObject().getString(Keywords.TYPE);

                    }

                // 4.9.2.
                } else {

                    typeLanguage = Keywords.TYPE;
                    typeLanguageValue = Keywords.ID;

                    containers.add(Keywords.ID);
                    containers.add(Keywords.ID.concat(Keywords.SET));
                    containers.add(Keywords.TYPE);
                    containers.add(Keywords.SET.concat(Keywords.TYPE));
                }

                // 4.9.3.
                containers.add(Keywords.SET);
            }

            // 4.10.
            containers.add(Keywords.NONE);

            // 4.11.
            if (!activeContext.inMode(JsonLdVersion.V1_0)
                    && (JsonUtils.isNotObject(value)
                            || !value.asJsonObject().containsKey(Keywords.INDEX))
                    ) {
                containers.add(Keywords.INDEX);
                containers.add(Keywords.INDEX.concat(Keywords.SET));
            }

            // 4.12.
            if (!activeContext.inMode(JsonLdVersion.V1_0)
                    && JsonUtils.isObject(value)
                    && value.asJsonObject().containsKey(Keywords.VALUE)
                    && value.asJsonObject().size() == 1
                    ) {

                containers.add(Keywords.LANGUAGE);
                containers.add(Keywords.LANGUAGE.concat(Keywords.SET));
            }

            // 4.13.
            if (typeLanguageValue == null) {
                typeLanguageValue = Keywords.NULL;
            }

            // 4.14.
            Collection<String> preferredValues = new ArrayList<>();

            // 4.15.
            if (Keywords.REVERSE.equals(typeLanguageValue)) {
                preferredValues.add(Keywords.REVERSE);
            }

            // 4.16.
            if ((Keywords.REVERSE.equals(typeLanguageValue) || Keywords.ID.equals(typeLanguageValue))
                   && JsonUtils.isObject(value)
                   && value.asJsonObject().containsKey(Keywords.ID)
                   ) {

                final JsonValue idValue = value.asJsonObject().get(Keywords.ID);

                // json-ld-star
                if (activeContext.getOptions().isRdfStar() && NodeObject.isEmbeddedNode(idValue)) {
                    preferredValues.add(Keywords.ID);
                    preferredValues.add(Keywords.VOCAB);

                } else if (JsonUtils.isString(idValue)) {
                    // 4.16.1.
                    final String idString = ((JsonString)idValue).getString();

                    final String compactedIdValue = activeContext.uriCompaction().vocab(true).compact(idString);

                    final Optional<TermDefinition> compactedIdValueTermDefinition = activeContext.getTerm(compactedIdValue);

                    if (compactedIdValueTermDefinition
                            .map(TermDefinition::getUriMapping)
                            .filter(idString::equals)
                            .isPresent()
                            ) {
                        preferredValues.add(Keywords.VOCAB);
                        preferredValues.add(Keywords.ID);

                    // 4.16.2.
                    } else {
                        preferredValues.add(Keywords.ID);
                        preferredValues.add(Keywords.VOCAB);
                    }

                } else {
                  throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ID_VALUE, "An @id entry was encountered whose value was not a string but [" + idValue + "].");
                }

                preferredValues.add(Keywords.NONE);

            // 4.17.
            } else {

                preferredValues.add(typeLanguageValue);
                preferredValues.add(Keywords.NONE);

                if (ListObject.isListObject(value)
                        && JsonUtils.isEmptyArray(value.asJsonObject().get(Keywords.LIST))) {

                    typeLanguage = Keywords.ANY;
                }
            }

            // 4.18.
            preferredValues.add(Keywords.ANY);

            // 4.19.
            for (final String preferredValue : new ArrayList<>(preferredValues)) {

                int index = preferredValue.indexOf('_');

                if (index == -1) {
                    continue;
                }

                preferredValues.add(preferredValue.substring(index));
            }

            // 4.20.
            final Optional<String> term = activeContext.termSelector(variable, containers, typeLanguage).match(preferredValues);

            // 4.21.
            if (term.isPresent()) {
                return term.get();
            }
        }

        // 5., 5.1.
        if ((vocab && activeContext.getVocabularyMapping() != null)
                && (variable.startsWith(activeContext.getVocabularyMapping())
                    && variable.length() > activeContext.getVocabularyMapping().length())) {

            String suffix = variable.substring(activeContext.getVocabularyMapping().length());

            if (!activeContext.containsTerm(suffix)) {
                return suffix;
            }
        }

        // 6.
        String compactUri = null;

        // 7.
        for (Entry<String, TermDefinition> termEntry : activeContext.getTermsMapping().entrySet()) {

            TermDefinition termDefinition = termEntry.getValue();

            // 7.1.
            if (termDefinition.getUriMapping() == null
                    || variable.equals(termDefinition.getUriMapping())
                    || !variable.startsWith(termDefinition.getUriMapping())
                    || termDefinition.isNotPrefix()
                    ) {
                continue;
            }

            // 7.2.
            String compactUriCandidate =
                            termEntry.getKey()
                                    .concat(":")
                                    .concat(variable.substring(termDefinition.getUriMapping().length()));

            // 7.3.
            if (((compactUri == null || (compactUriCandidate.compareTo(compactUri) < 0))
                            && !activeContext.containsTerm(compactUriCandidate))
                    || (activeContext
                                .getTerm(compactUriCandidate)
                                .map(TermDefinition::getUriMapping)
                                .filter(u -> u.equals(variable))
                                .isPresent()
                            && JsonUtils.isNull(value)
                            )
                    ) {
                compactUri = compactUriCandidate;
            }
        }

        /// 8.
        if (compactUri != null) {
            return compactUri;
        }

        // 9.
        try {
            final URI uri = URI.create(variable);

            if (uri != null
                    && uri.isAbsolute()
                    && uri.getScheme() != null
                    && uri.getAuthority() == null
                    && activeContext.getTerm(uri.getScheme()).filter(TermDefinition::isPrefix).isPresent()
                ) {
                    throw new JsonLdError(JsonLdErrorCode.IRI_CONFUSED_WITH_PREFIX);
            }
        } catch (IllegalArgumentException e) { /* variable is not URI */ }

        // 10.
        if (!vocab && activeContext.getBaseUri() != null && !BlankNode.hasPrefix(variable)) {
             final String relativeUri = UriRelativizer.relativize(activeContext.getBaseUri(), variable);

             return Keywords.matchForm(relativeUri) ? "./".concat(relativeUri) : relativeUri;
        }

        // 11.
        return variable;
    }
}
