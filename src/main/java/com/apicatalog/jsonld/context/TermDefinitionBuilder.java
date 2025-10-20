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
package com.apicatalog.jsonld.context;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.CompactUri;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.jsonld.uri.UriValidationPolicy;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#create-term-definition">Create
 *      Term Definition</a>
 *
 */
public final class TermDefinitionBuilder {

    private static final Logger LOGGER = Logger.getLogger(TermDefinitionBuilder.class.getName());

    private static final Collection<String> CONTAINER_KEYWORDS = Arrays.asList(
            Keywords.GRAPH,
            Keywords.ID,
            Keywords.INDEX,
            Keywords.LANGUAGE,
            Keywords.LIST,
            Keywords.SET,
            Keywords.TYPE);

    private static final Collection<String> PROTECTED_KEYWORDS = Arrays.asList(
            Keywords.ID,
            Keywords.REVERSE,
            Keywords.CONTAINER,
            Keywords.CONTEXT,
            Keywords.DIRECTION,
            Keywords.INDEX,
            Keywords.LANGUAGE,
            Keywords.NEST,
            Keywords.PREFIX,
            Keywords.PROTECTED,
            Keywords.TYPE);

    // mandatory
    private final ActiveContext activeContext;

    private final Object localContext;
    private final NodeAdapter adapter;

    private final Map<String, Boolean> defined;

    // optional
    private URI baseUrl;

    private boolean protectedFlag;

    private boolean overrideProtectedFlag;

    private Collection<String> remoteContexts;

    private TermDefinitionBuilder(ActiveContext activeContext, Object localContext, NodeAdapter adapter, Map<String, Boolean> defined) {
        this.activeContext = activeContext;
        this.localContext = localContext;
        this.adapter = adapter;
        this.defined = defined;

        // default values
        this.baseUrl = null;
        this.protectedFlag = false;
        this.overrideProtectedFlag = false;
        this.remoteContexts = new ArrayList<>();
    }

    public static final TermDefinitionBuilder with(ActiveContext activeContext, Object localContext, NodeAdapter adapter, Map<String, Boolean> defined) {
        return new TermDefinitionBuilder(activeContext, localContext, adapter, defined);
    }

    public void create(final String term) throws JsonLdError, IOException {

        if (term == null || term.isBlank()) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 1.
        if (defined.containsKey(term)) {

            if (defined.get(term)) {
                return;
            }

            throw new JsonLdError(JsonLdErrorCode.CYCLIC_IRI_MAPPING);
        }

        // 2.
        defined.put(term, false);

        // 3.
        var value = adapter.property(term, localContext);

        // 4.
        if (Keywords.TYPE.equals(term)) {

            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
            }

            if (adapter.isMap(value)) {

                var container = adapter.property(Keywords.CONTAINER, value);
                var protect = adapter.property(Keywords.PROTECTED, value);

                if (container != null && adapter.isSingleEntry(value)) {

                    if (!adapter.isString(container) || !Keywords.SET.equals(adapter.stringValue(container))) {
                        throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (container != null
                        && protect != null
                        && adapter.keys(value).size() == 2) {

                    if (adapter.isString(container) && !Keywords.SET.equals(adapter.stringValue(container))
                            || adapter.isCollection(container) && adapter.elementStream(container)
                                    .map(adapter::asString)
                                    .noneMatch(Keywords.SET::equals)
                            || adapter.isMap(container) && adapter.keyStream(container)
                                    .map(adapter::asString)
                                    .noneMatch(Keywords.SET::equals)) {
                        throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (protect == null || adapter.keys(value).size() != 1) {
                    throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
            }

        } else if (Keywords.contains(term)) {
            // 5.
            throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION, "A keyword [" + term + "] redefinition has been detected.");

        } else if (Keywords.matchForm(term)) {
            LOGGER.log(Level.WARNING, "Term [{0}] has form of a keyword. Keywords cannot be overridden.", term);
            return;
        }

        // 6.
        final var previousDefinition = activeContext.removeTerm(term).orElse(null);

        final Function<String, ?> propertyGetter;
        final Collection<String> valueObjectKeys;
        final boolean simpleTerm;
        final Object idValue;

        // 7.
        if (adapter.isNull(value)) {
            propertyGetter = x -> null;
            valueObjectKeys = Collections.emptySet();
            idValue = false;
            simpleTerm = false;

        } else if (adapter.isString(value)) {
            // 8.
            propertyGetter = x -> null;
            valueObjectKeys = Collections.emptySet();
            idValue = value;
            simpleTerm = true;

        } else if (adapter.isMap(value)) {
            // 9.
            propertyGetter = name -> adapter.property(name, value);
            valueObjectKeys = adapter
                    .keyStream(value)
                    .map(adapter::stringValue)
                    .toList();
            idValue = adapter.property(Keywords.ID, value);
            simpleTerm = false;

        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 10.
        final var definition = new TermDefinition(false, protectedFlag, false);

        // 11.
        final var protectedValue = propertyGetter.apply(Keywords.PROTECTED);

        if (protectedValue != null) {

            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            if (!adapter.isBoolean(protectedValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROTECTED_VALUE);
            }

            definition.setProtected(adapter.isTrue(protectedValue));
        }

        // 12.
        final var typeValue = propertyGetter.apply(Keywords.TYPE);

        if (typeValue != null) {

            if (!adapter.isString(typeValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.2.
            final String expandedTypeString = activeContext
                    .uriExpansion()
                    .localContext(localContext, adapter)
                    .defined(defined)
                    .vocab(true)
                    .expand(adapter.stringValue(typeValue));

            if (expandedTypeString == null) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.3.
            if (((Keywords.JSON.equals(expandedTypeString) || Keywords.NONE.equals(expandedTypeString))
                    && activeContext.runtime().isV10())
                    // 12.4.
                    || (Keywords.noneMatch(expandedTypeString, Keywords.ID, Keywords.JSON, Keywords.NONE, Keywords.VOCAB)
                            && UriUtils.isNotAbsoluteUri(expandedTypeString, UriValidationPolicy.Full))) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.5.
            definition.setTypeMapping(expandedTypeString);
        }

        // 13.
        final var reverseValue = propertyGetter.apply(Keywords.REVERSE);

        if (reverseValue != null) {

            // 13.1.
            if (valueObjectKeys.contains(Keywords.ID) || valueObjectKeys.contains(Keywords.NEST)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
            }

            // 13.2.
            if (!adapter.isString(reverseValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
            }

            final String reverseString = adapter.stringValue(reverseValue);

            // 13.3.
            if (Keywords.matchForm(reverseString)) {
                LOGGER.log(Level.WARNING, "The value [{0}] associated with @reverse cannot have form of a keyword.", reverseString);
                return;
            }

            // 13.4.
            definition.setUriMapping(
                    activeContext
                            .uriExpansion()
                            .localContext(localContext, adapter)
                            .defined(defined)
                            .vocab(true)
                            .expand(reverseString));

            if (UriUtils.isNotURI(definition.getUriMapping())) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
            }

            // 13.5.
            final var containerValue = propertyGetter.apply(Keywords.CONTAINER);

            if (containerValue != null) {

                if (!adapter.isString(containerValue) && !adapter.isNull(containerValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
                }

                if (adapter.isString(containerValue)) {

                    final String containerString = adapter.stringValue(containerValue);

                    if (Keywords.SET.equals(containerString) || Keywords.INDEX.equals(containerString)) {
                        definition.addContainerMapping(containerString);

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
                    }
                }
            }

            // 13.6.
            definition.setReverseProperty(true);

            // 13.7.
            activeContext.setTerm(term, definition);
            defined.put(term, Boolean.TRUE);

            // 14.
        } else if (idValue != null
                && (!adapter.isString(idValue)
                        || !term.equals(adapter.stringValue(idValue)))) {

            // 14.1.
            if (!adapter.isNull(idValue) && !Boolean.FALSE.equals(idValue)) {

                // 14.2.1
                if (!adapter.isString(idValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                }

                final String idValueString = adapter.stringValue(idValue);

                // 14.2.2
                if (!Keywords.contains(idValueString) && Keywords.matchForm(idValueString)) {
                    LOGGER.log(Level.WARNING, "The value [{0}] associated with @id has form of a keyword but is not keyword.", idValueString);
                    return;
                }

                // 14.2.3
                definition.setUriMapping(
                        activeContext
                                .uriExpansion()
                                .localContext(localContext, adapter)
                                .defined(defined)
                                .vocab(true)
                                .expand(idValueString));

                if (Keywords.CONTEXT.equals(definition.getUriMapping())) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ALIAS);
                }

                if (!Keywords.contains(definition.getUriMapping()) && UriUtils.isNotURI(definition.getUriMapping())
                        && !BlankNode.hasPrefix(definition.getUriMapping())) {

                    throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                }

                // 14.2.4
                if (term.substring(0, term.length() - 1).indexOf(':', 1) != -1 || term.contains("/")) {

                    // 14.2.4.1
                    defined.put(term, Boolean.TRUE);

                    // 14.2.4.2
                    final String expandedTerm = activeContext
                            .uriExpansion()
                            .localContext(localContext, adapter)
                            .defined(defined)
                            .vocab(true)
                            .expand(term);

                    if (expandedTerm == null || !expandedTerm.equals(definition.getUriMapping())) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                    }
                }

                // 14.2.5
                if (definition.isNotPrefix()) {
                    definition.setPrefix(!term.contains(":")
                            && !term.contains("/")
                            && simpleTerm
                            && (definition.getUriMapping() != null
                                    && ((UriUtils.endsWithGenDelim(definition.getUriMapping())
                                            && UriUtils.isURI(definition.getUriMapping().substring(0, definition.getUriMapping().length() - 1)))
                                            || BlankNode.hasPrefix(definition.getUriMapping()))));
                }
            }

            // 15.
        } else if (term.indexOf(':', 1)  != -1) {

            final CompactUri compactUri = CompactUri.of(term);

            // 15.1.
            if (compactUri != null
                    && compactUri.isNotBlank()
                    && adapter.keys(localContext).contains(compactUri.prefix())) {

                activeContext.newTerm(localContext, adapter, defined).create(compactUri.prefix());
            }
            // 15.2.
            if (compactUri != null && compactUri.isNotBlank() && activeContext.containsTerm(compactUri.prefix())) {

                definition.setUriMapping(
                        activeContext
                                .findTerm(compactUri.prefix())
                                .map(TermDefinition::getUriMapping)
                                .map(u -> u.concat(compactUri.suffix()))
                                .orElse(null));

                // 15.3.
            } else if (UriUtils.isURI(term) || BlankNode.hasPrefix(term)) {
                definition.setUriMapping(term);
            }

            // 16.
        } else if (term.contains("/")) {

            definition.setUriMapping(
                    activeContext
                            .uriExpansion()
                            .localContext(localContext, adapter)
                            .defined(defined)
                            .vocab(true)
                            .expand(term));

            if (UriUtils.isNotURI(definition.getUriMapping())) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
            }

            // 17.
        } else if (Keywords.TYPE.equals(term)) {
            definition.setUriMapping(Keywords.TYPE);

            // 18.
        } else if (activeContext.getVocabularyMapping() == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);

        } else {
            definition.setUriMapping(activeContext.getVocabularyMapping().concat(term));
        }

        // 19.
        var containerValue = propertyGetter.apply(Keywords.CONTAINER);

        if (containerValue != null) {

            if (!validateContainer(containerValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTAINER_MAPPING);
            }

            // 19.3.
            adapter.asStream(containerValue)
                    .filter(adapter::isString)
                    .map(adapter::stringValue)
                    .forEach(definition::addContainerMapping);

            // 19.4.
            if (definition.getContainerMapping().contains(Keywords.TYPE)) {

                // 19.4.1.
                if (definition.getTypeMapping() == null) {
                    definition.setTypeMapping(Keywords.ID);
                }

                if (!Keywords.ID.equals(definition.getTypeMapping())
                        && !Keywords.VOCAB.equals(definition.getTypeMapping())) {

                    throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
                }
            }
        }

        // 20.
        if (valueObjectKeys.contains(Keywords.INDEX)) {

            // 20.1.
            if (activeContext.runtime().isV10() || !definition.getContainerMapping().contains(Keywords.INDEX)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            // 20.2.
            final var index = propertyGetter.apply(Keywords.INDEX);

            if (!adapter.isString(index)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            final var indexString = adapter.stringValue(index);

            final var expandedIndex = activeContext
                    .uriExpansion()
                    .localContext(localContext, adapter)
                    .defined(defined)
                    .vocab(true)
                    .expand(indexString);

            if (expandedIndex == null || UriUtils.isNotURI(expandedIndex)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            definition.setIndexMapping(indexString);
        }

        // 21.
        final var contextValue = propertyGetter.apply(Keywords.CONTEXT);

        if (contextValue != null) {

            // 21.1.
            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            // 21.3.
            try {
                activeContext
                        .newContext()
                        .overrideProtected(true)
                        .remoteContexts(new ArrayList<>(remoteContexts))
                        .validateScopedContext(false)
                        .build(contextValue, adapter, baseUrl);

            } catch (JsonLdError e) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_SCOPED_CONTEXT, e);
            }

            // 21.4.
            definition.setLocalContext(contextValue, adapter);
            definition.setBaseUrl(baseUrl);
        }

        // 22.
        final var languageValue = propertyGetter.apply(Keywords.LANGUAGE);

        if (languageValue != null && !valueObjectKeys.contains(Keywords.TYPE)) {

            if (adapter.isNull(languageValue)) {
                definition.setLanguageMapping(Keywords.NULL);

            } else if (adapter.isString(languageValue)) {

                final var language = adapter.stringValue(languageValue);

                if (!LanguageTag.isWellFormed(language)) {
                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", language);
                }

                definition.setLanguageMapping(language);

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAPPING);
            }

//            if (JsonUtils.isNull(language) || JsonUtils.isString(language)) {
//
//                if (JsonUtils.isString(language) && !LanguageTag.isWellFormed(((JsonString) language).getString())) {
//                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", ((JsonString) language).getString());
//                }
//
//                definition.setLanguageMapping(language);
//
//            } else {
//                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAPPING);
//            }
        }

        // 23.
        final var directionValue = propertyGetter.apply(Keywords.DIRECTION);

        if (directionValue != null
                && !valueObjectKeys.contains(Keywords.TYPE)) {

            if (adapter.isNull(directionValue)) {
                definition.setDirectionMapping(DirectionType.NULL);

            } else if (adapter.isString(directionValue)) {

                var directionString = adapter.stringValue(directionValue);

                if ("ltr".equals(directionString)) {
                    definition.setDirectionMapping(DirectionType.LTR);

                } else if ("rtl".equals(directionString)) {
                    definition.setDirectionMapping(DirectionType.RTL);

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
            }
        }

        // 24.
        final var nestValue = propertyGetter.apply(Keywords.NEST);

        if (nestValue != null) {

            // 24.1
            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            if (!adapter.isString(nestValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
            }

            var nestString = adapter.stringValue(nestValue);

            if (Keywords.contains(nestString) && !Keywords.NEST.equals(nestString)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
            }

            definition.setNestValue(nestString);
        }

        // 25.
        final var prefixValue = propertyGetter.apply(Keywords.PREFIX);

        if (prefixValue != null) {

            // 25.1.
            if (activeContext.runtime().isV10() || term.contains(":") || term.contains("/")) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            final var prefixType = adapter.type(prefixValue);

            if (prefixType == NodeType.TRUE) {
                definition.setPrefix(true);

            } else if (prefixType == NodeType.FALSE) {
                definition.setPrefix(false);

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PREFIX_VALUE);
            }

            // 25.3
            if (definition.isPrefix() && Keywords.contains(definition.getUriMapping())) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }
        }

        // 26.
        if (!PROTECTED_KEYWORDS.containsAll(valueObjectKeys)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 27.
        if (!overrideProtectedFlag && previousDefinition != null && previousDefinition.isProtected()) {

            // 27.1.
            if (definition.isNotSameExcept(previousDefinition)) {
                throw new JsonLdError(JsonLdErrorCode.PROTECTED_TERM_REDEFINITION);
            }

            // 27.2.
            activeContext.setTerm(term, previousDefinition);

        } else {
            activeContext.setTerm(term, definition);
        }

        defined.put(term, true);
    }

    public TermDefinitionBuilder baseUrl(URI baseUrl) {
        this.baseUrl = baseUrl;
        return this;
    }

    public TermDefinitionBuilder protectedFlag(boolean protectedFlag) {
        this.protectedFlag = protectedFlag;
        return this;
    }

    public TermDefinitionBuilder overrideProtectedFlag(boolean overrideProtectedFlag) {
        this.overrideProtectedFlag = overrideProtectedFlag;
        return this;
    }

    public TermDefinitionBuilder remoteContexts(Collection<String> remoteContexts) {
        this.remoteContexts = remoteContexts;
        return this;
    }

    private final boolean validateContainer(final Object value) {

        if (adapter.isNull(value)) {
            return false;
        }

        var container = value;

        if (activeContext.runtime().isV10()) {

            return adapter.isString(container)
                    && Keywords.noneMatch(
                            adapter.stringValue(container),
                            Keywords.GRAPH,
                            Keywords.ID,
                            Keywords.TYPE);
        }

        if (adapter.isSingleElement(container)) {
            container = adapter.singleElement(container);
        }

        if (adapter.isString(container)) {
            return CONTAINER_KEYWORDS.contains(adapter.stringValue(container));
        }

        return adapter.isCollection(container)
                && validateContainerArray(adapter.elementStream(container)
                        .filter(adapter::isString)
                        .map(adapter::stringValue)
                        .toList());
    }

    private static final boolean validateContainerArray(final Collection<String> containers) {

        if (containers.size() > 3) {
            return false;
        }

        if (containers.contains(Keywords.GRAPH)
                && (containers.contains(Keywords.ID)
                        || containers.contains(Keywords.INDEX))) {

            return containers.size() == 2 || containers.contains(Keywords.SET);
        }

        return containers.size() == 2
                && containers.contains(Keywords.SET)
                && (containers.contains(Keywords.GRAPH)
                        || containers.contains(Keywords.ID)
                        || containers.contains(Keywords.INDEX)
                        || containers.contains(Keywords.LANGUAGE)
                        || containers.contains(Keywords.TYPE));
    }

}