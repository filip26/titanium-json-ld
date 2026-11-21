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

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.SequencedCollection;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.lang.JsonLdAdapter;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.uri.BlankNode;
import com.apicatalog.web.uri.CompactUri;
import com.apicatalog.web.uri.UriUtils;
import com.apicatalog.web.uri.UriValidationPolicy;

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

    private final Map<String, ?> localContext;

    private final Map<String, Boolean> defined;
    private final DocumentLoader loader;
    private final Execution runtime;

    // optional
    private URI baseUrl;

    private boolean protectedFlag;

    private boolean overrideProtectedFlag;

    private Collection<String> remoteContexts;

    private TermDefinitionBuilder(
            ActiveContext activeContext,
            Map<String, ?> localContext,
            Map<String, Boolean> defined,
            DocumentLoader loader,
            Execution runtime) {
        this.activeContext = activeContext;
        this.localContext = localContext;
        this.defined = defined;
        this.loader = loader;
        this.runtime = runtime;

        // default values
        this.baseUrl = null;
        this.protectedFlag = false;
        this.overrideProtectedFlag = false;
        this.remoteContexts = new ArrayList<>();
    }

    public static final TermDefinitionBuilder with(
            ActiveContext activeContext,
            Map<String, ?> localContext,
            Map<String, Boolean> defined,
            DocumentLoader loader,
            Execution runtime) {
        return new TermDefinitionBuilder(activeContext, localContext, defined, loader, runtime);
    }

    public void create(final String term) throws JsonLdException {

        if (term == null || term.isBlank()) {
            throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
        }

        // 1.
        if (defined.containsKey(term)) {

            if (defined.get(term)) {
                return;
            }

            throw new JsonLdException(ErrorCode.CYCLIC_IRI_MAPPING);
        }

        // 2.
        defined.put(term, false);

        // 3.
        var value = localContext.get(term);

        // 4.
        if (Keywords.TYPE.equals(term)) {

            if (activeContext.isV10()) {
                throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION);
            }

            if (value instanceof Map map) {

                var container = map.get(Keywords.CONTAINER);
                var protect = map.get(Keywords.PROTECTED);

                if (container != null && map.size() == 1) {

                    if (!Keywords.SET.equals(container)) {
                        throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (container != null
                        && protect != null
                        && map.size() == 2) {

                    if (container instanceof String && !Keywords.SET.equals(container)
                            || container instanceof Collection<?> containerCol
                                    && !containerCol.contains(Keywords.SET)
                            || container instanceof Map containerMap
                                    && !containerMap.containsKey(Keywords.SET)) {
                        throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (protect == null || map.size() != 1) {
                    throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION);
                }

            } else {
                throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION);
            }

        } else if (Keywords.contains(term)) {
            // 5.
            throw new JsonLdException(ErrorCode.KEYWORD_REDEFINITION,
                    "A keyword [" + term + "] redefinition has been detected.");

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
        if (value == null) {
            propertyGetter = x -> null;
            valueObjectKeys = Set.of();
            idValue = false;
            simpleTerm = false;

        } else if (value instanceof String) {
            // 8.
            propertyGetter = x -> null;
            valueObjectKeys = Set.of();
            idValue = value;
            simpleTerm = true;

        } else if (value instanceof Map map) {
            // 9.
            propertyGetter = name -> map.get(name);
            valueObjectKeys = map.keySet();
            if (map.containsKey(Keywords.ID)) {
                var id = map.get(Keywords.ID);
                idValue = id != null ? id : false;
            } else {
                idValue = null;
            }
            simpleTerm = false;

        } else {
            throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
        }

        // 10.
        final var definition = new TermDefinition(false, protectedFlag, false);

        // 11.
        if (valueObjectKeys.contains(Keywords.PROTECTED)) {

            final var protectedValue = propertyGetter.apply(Keywords.PROTECTED);

            if (activeContext.isV10()) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }

            if (!(protectedValue instanceof Boolean protectedBool)) {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_PROTECTED_VALUE);
            }

            definition.setProtected(protectedBool);
        }

        // 12.
        if (valueObjectKeys.contains(Keywords.TYPE)) {

            final var typeValue = propertyGetter.apply(Keywords.TYPE);

            if (!(typeValue instanceof String typeString)) {
                throw new JsonLdException(ErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.2.
            final String expandedTypeString = UriExpansion.with(activeContext, loader, runtime)
                    .localContext(localContext)
                    .defined(defined)
                    .vocab(true)
                    .expand(typeString);

            if (expandedTypeString == null) {
                throw new JsonLdException(ErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.3.
            if (((Keywords.JSON.equals(expandedTypeString) || Keywords.NONE.equals(expandedTypeString))
                    && activeContext.isV10())
                    // 12.4.
                    || (Keywords.noneMatch(expandedTypeString, Keywords.ID, Keywords.JSON, Keywords.NONE,
                            Keywords.VOCAB)
                            && UriUtils.isNotAbsoluteUri(expandedTypeString, UriValidationPolicy.Full))) {
                throw new JsonLdException(ErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.5.
            definition.setTypeMapping(expandedTypeString);
        }

        // 13.
        if (valueObjectKeys.contains(Keywords.REVERSE)) {
            // 13.1.
            if (valueObjectKeys.contains(Keywords.ID) || valueObjectKeys.contains(Keywords.NEST)) {
                throw new JsonLdException(ErrorCode.INVALID_REVERSE_PROPERTY);
            }

            final var reverseValue = propertyGetter.apply(Keywords.REVERSE);

            // 13.2.
            if (!(reverseValue instanceof String reverseString)) {
                throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
            }

            // 13.3.
            if (Keywords.matchForm(reverseString)) {
                LOGGER.log(Level.WARNING, "The value [{0}] associated with @reverse cannot have form of a keyword.",
                        reverseString);
                return;
            }

            // 13.4.
            definition.setUriMapping(
                    UriExpansion.with(activeContext, loader, runtime)
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(reverseString));

            if (UriUtils.isNotURI(definition.getUriMapping())) {
                throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
            }

            // 13.5.
            final var containerValue = propertyGetter.apply(Keywords.CONTAINER);

            if (containerValue instanceof String containerString
                    && (Keywords.SET.equals(containerString)
                            || Keywords.INDEX.equals(containerString))) {
                definition.addContainerMapping(containerString);

            } else if (valueObjectKeys.contains(Keywords.CONTAINER)) {
                throw new JsonLdException(ErrorCode.INVALID_REVERSE_PROPERTY,
                        "The @reverse value=" + containerValue + " is invalid");
            }

            // 13.6.
            definition.setReverseProperty(true);

            // 13.7.
            activeContext.setTerm(term, definition);
            defined.put(term, Boolean.TRUE);

            // 14.
        } else if (idValue != null
                && (!(idValue instanceof String idString)
                        || !term.equals(idString))) {

            // 14.1.
            if (idValue != null && !Boolean.FALSE.equals(idValue)) {

                if (idValue instanceof String idValueString) {

                    // 14.2.2
                    if (!Keywords.contains(idValueString) && Keywords.matchForm(idValueString)) {
                        LOGGER.log(Level.WARNING,
                                "The value [{0}] associated with @id has form of a keyword but is not keyword.",
                                idValueString);
                        return;
                    }

                    // 14.2.3
                    definition.setUriMapping(
                            UriExpansion.with(activeContext, loader, runtime)
                                    .localContext(localContext)
                                    .defined(defined)
                                    .vocab(true)
                                    .expand(idValueString));
                } else {
                    throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
                }

                if (Keywords.CONTEXT.equals(definition.getUriMapping())) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_ALIAS);
                }

                if (!Keywords.contains(definition.getUriMapping()) && UriUtils.isNotURI(definition.getUriMapping())
                        && !BlankNode.hasPrefix(definition.getUriMapping())) {

                    throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
                }

                // 14.2.4
                if (term.substring(0, term.length() - 1).indexOf(':', 1) != -1 || term.contains("/")) {

                    // 14.2.4.1
                    defined.put(term, Boolean.TRUE);

                    // 14.2.4.2
                    final var expandedTerm = UriExpansion.with(activeContext, loader, runtime)
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(term);

                    if (expandedTerm == null || !expandedTerm.equals(definition.getUriMapping())) {
                        throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
                    }
                }

                // 14.2.5
                if (definition.isNotPrefix()) {
                    definition.setPrefix(!term.contains(":")
                            && !term.contains("/")
                            && simpleTerm
                            && (definition.getUriMapping() != null
                                    && ((UriUtils.endsWithGenDelim(definition.getUriMapping())
                                            && UriUtils.isURI(definition.getUriMapping().substring(0,
                                                    definition.getUriMapping().length() - 1)))
                                            || BlankNode.hasPrefix(definition.getUriMapping()))));
                }
            }

            // 15.
        } else if (term.indexOf(':', 1) != -1) {

            final CompactUri compactUri = CompactUri.of(term);

            // 15.1.
            if (compactUri != null
                    && compactUri.isNotBlank()
                    && localContext.containsKey(compactUri.prefix())) {

                activeContext
                        .newTerm(
                                compactUri.prefix(),
                                localContext,
                                defined,
                                loader,
                                runtime);
            }
            // 15.2.
            if (compactUri != null
                    && compactUri.isNotBlank()
                    && activeContext.containsTerm(compactUri.prefix())) {

                definition.setUriMapping(
                        activeContext
                                .findTerm(compactUri.prefix())
                                .map(TermDefinition::getUriMapping)
                                .map(u -> u + compactUri.suffix())
                                .orElse(null));

                // 15.3.
            } else if (UriUtils.isURI(term) || BlankNode.hasPrefix(term)) {
                definition.setUriMapping(term);
            }

            // 16.
        } else if (term.contains("/")) {

            definition.setUriMapping(
                    UriExpansion.with(activeContext, loader, runtime)
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(term));

            if (UriUtils.isNotURI(definition.getUriMapping())) {
                throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);
            }

            // 17.
        } else if (Keywords.TYPE.equals(term)) {
            definition.setUriMapping(Keywords.TYPE);

            // 18.
        } else if (activeContext.getVocabularyMapping() == null) {
            throw new JsonLdException(ErrorCode.INVALID_IRI_MAPPING);

        } else {
            definition.setUriMapping(activeContext.getVocabularyMapping().concat(term));
        }

        // 19.
        if (valueObjectKeys.contains(Keywords.CONTAINER)) {

            var containerValue = propertyGetter.apply(Keywords.CONTAINER);

            if (!validateContainer(containerValue)) {
                throw new JsonLdException(ErrorCode.INVALID_CONTAINER_MAPPING);
            }

            // 19.3.
            for (var mapping : JsonLdAdapter.asCollection(containerValue)) {
                if (mapping instanceof String mappingString) {
                    definition.addContainerMapping(mappingString);
                } else {
                    throw new IllegalStateException("TODO");
                }
            }

            // 19.4.
            if (definition.getContainerMapping().contains(Keywords.TYPE)) {

                // 19.4.1.
                if (definition.getTypeMapping() == null) {
                    definition.setTypeMapping(Keywords.ID);
                }

                if (!Keywords.ID.equals(definition.getTypeMapping())
                        && !Keywords.VOCAB.equals(definition.getTypeMapping())) {

                    throw new JsonLdException(ErrorCode.INVALID_TYPE_MAPPING);
                }
            }
        }

        // 20.
        if (valueObjectKeys.contains(Keywords.INDEX)) {

            // 20.1.
            if (activeContext.isV10() || !definition.getContainerMapping().contains(Keywords.INDEX)) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }

            // 20.2.
            if (valueObjectKeys.contains(Keywords.INDEX)) {

                if (propertyGetter.apply(Keywords.INDEX) instanceof String indexString) {

                    final var expandedIndex = UriExpansion.with(activeContext, loader, runtime)
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(indexString);

                    if (expandedIndex == null || UriUtils.isNotURI(expandedIndex)) {
                        throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
                    }

                    definition.setIndexMapping(indexString);

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
                }
            }
        }

        // 21.
        if (valueObjectKeys.contains(Keywords.CONTEXT)) {

            // 21.1.
            if (activeContext.isV10()) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }

            final var contextValue = propertyGetter.apply(Keywords.CONTEXT);

            // 21.3.
            try {
                activeContext
                        .newContext(loader, runtime)
                        .overrideProtected(true)
                        .remoteContexts(new ArrayList<>(remoteContexts))
                        .validateScopedContext(false)
                        .build(contextValue, baseUrl);

            } catch (JsonLdException e) {
                throw new JsonLdException(ErrorCode.INVALID_SCOPED_CONTEXT, e);
            }

            // 21.4.
            definition.setLocalContext(contextValue);
            definition.setBaseUrl(baseUrl);
        }

        // 22.
        if (valueObjectKeys.contains(Keywords.LANGUAGE) && !valueObjectKeys.contains(Keywords.TYPE)) {

            final var languageValue = propertyGetter.apply(Keywords.LANGUAGE);

            if (languageValue == null) {
                definition.setLanguageMapping(Keywords.NULL);

            } else if (languageValue instanceof String language) {

                if (!LanguageTag.isWellFormed(language)) {
                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", language);
                }

                definition.setLanguageMapping(language);

            } else {
                throw new JsonLdException(ErrorCode.INVALID_LANGUAGE_MAPPING);
            }
        }

        // 23.

        if (valueObjectKeys.contains(Keywords.DIRECTION) && !valueObjectKeys.contains(Keywords.TYPE)) {

            final var directionValue = propertyGetter.apply(Keywords.DIRECTION);

            if (directionValue == null) {
                definition.setDirectionMapping(Direction.NULL);

            } else if (directionValue instanceof String directionString) {

                if ("ltr".equals(directionString)) {
                    definition.setDirectionMapping(Direction.LTR);

                } else if ("rtl".equals(directionString)) {
                    definition.setDirectionMapping(Direction.RTL);

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
                }

            } else {
                throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
            }
        }

        // 24.
        if (valueObjectKeys.contains(Keywords.NEST)) {

            // 24.1
            if (activeContext.isV10()) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }

            if (propertyGetter.apply(Keywords.NEST) instanceof String nestString) {

                if (Keywords.contains(nestString) && !Keywords.NEST.equals(nestString)) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_NEST_VALUE);
                }

                definition.setNestValue(nestString);
            } else {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_NEST_VALUE);
            }
        }

        // 25.
        if (valueObjectKeys.contains(Keywords.PREFIX)) {

            // 25.1.
            if (activeContext.isV10() || term.contains(":") || term.contains("/")) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }

            if (propertyGetter.apply(Keywords.PREFIX) instanceof Boolean prefix) {
                definition.setPrefix(prefix);

            } else {
                throw new JsonLdException(ErrorCode.INVALID_KEYWORD_PREFIX_VALUE);
            }

            // 25.3
            if (definition.isPrefix() && Keywords.contains(definition.getUriMapping())) {
                throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
            }
        }

        // 26.
        if (!PROTECTED_KEYWORDS.containsAll(valueObjectKeys)) {
            throw new JsonLdException(ErrorCode.INVALID_TERM_DEFINITION);
        }

        // 27.
        if (!overrideProtectedFlag && previousDefinition != null && previousDefinition.isProtected()) {

            // 27.1.
            if (definition.isNotSameExcept(previousDefinition)) {
                throw new JsonLdException(ErrorCode.PROTECTED_TERM_REDEFINITION);
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

        if (value == null) {
            return false;
        }

        var container = value;

        if (activeContext.isV10()) {
            return container instanceof String containerString
                    && Keywords.noneMatch(
                            containerString,
                            Keywords.GRAPH,
                            Keywords.ID,
                            Keywords.TYPE);
        }

        // unwrap single element
        if (container instanceof SequencedCollection<?> col && col.size() == 1) {
            container = col.getFirst();
        }

        if (container instanceof String containerString) {
            return CONTAINER_KEYWORDS.contains(containerString);
        }

        return container instanceof Collection col
                && validateContainerArray(col);
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