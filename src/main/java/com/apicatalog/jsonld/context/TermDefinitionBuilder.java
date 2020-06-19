package com.apicatalog.jsonld.context;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.CompactUri;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.uri.UriUtils;

/**
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#create-term-definition">Create
 *      Term Definition</a>
 *
 */
public final class TermDefinitionBuilder {

    private static final Logger LOGGER = Logger.getLogger(TermDefinitionBuilder.class.getName());
    
    // mandatory
    private final ActiveContext activeContext;

    private final JsonObject localContext;

    private final Map<String, Boolean> defined;

    // optional
    private URI baseUrl;

    private boolean protectedFlag;

    private boolean overrideProtectedFlag;

    private Collection<String> remoteContexts;

    private TermDefinitionBuilder(ActiveContext activeContext, JsonObject localContext, Map<String, Boolean> defined) {
        this.activeContext = activeContext;
        this.localContext = localContext;
        this.defined = defined;

        // default values
        this.baseUrl = null;
        this.protectedFlag = false;
        this.overrideProtectedFlag = false;
        this.remoteContexts = new ArrayList<>();
    }

    public static final TermDefinitionBuilder with(ActiveContext activeContext, JsonObject localContext, Map<String, Boolean> defined) {
        return new TermDefinitionBuilder(activeContext, localContext, defined);
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

    public void create(final String term) throws JsonLdError {

        if (term.isBlank()) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 1.
        if (defined.containsKey(term)) {
            if (Boolean.TRUE.equals(defined.get(term))) {
                return;
            }

            throw new JsonLdError(JsonLdErrorCode.CYCLIC_IRI_MAPPING);
        }

        // 2.
        defined.put(term, Boolean.FALSE);

        // 3.
        JsonValue value = localContext.get(term);

        // 4.
        if (Keywords.TYPE.equals(term)) {

            if (activeContext.inMode(Version.V1_0)) {
                throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
            }

            if (JsonUtils.isObject(value)) {

                JsonObject map = value.asJsonObject();

                if (map.size() == 1 && map.containsKey(Keywords.CONTAINER)) {

                    JsonValue container = map.get(Keywords.CONTAINER);
                    if (JsonUtils.isNotString(container)
                            || !Keywords.SET.equals(((JsonString) container).getString())) {
                        throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (map.size() == 2 && map.containsKey(Keywords.CONTAINER)
                        && map.containsKey(Keywords.PROTECTED)) {

                    JsonValue containerValue = map.get(Keywords.CONTAINER);
                    if (!JsonUtils.contains(Keywords.SET, containerValue)) {
                        throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                    }

                } else if (map.size() != 1 || !map.containsKey(Keywords.PROTECTED)) {
                    throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);
            }

        // 5.
        } else if (Keywords.contains(term)) {
            throw new JsonLdError(JsonLdErrorCode.KEYWORD_REDEFINITION);

        } else if (Keywords.matchForm(term)) {
            LOGGER.log(Level.WARNING, "Term [{0}] has form of a keyword. Keywords cannot be overridden.", term);
            return;
        }
        
        // 6.
        final Optional<TermDefinition> previousDefinition = activeContext.removeTerm(term);

        JsonObject valueObject = null;
        Boolean simpleTerm = null;

        // 7.
        if (JsonUtils.isNull(value)) {

            valueObject = Json.createObjectBuilder().add(Keywords.ID, JsonValue.NULL).build();

        // 8.
        } else if (JsonUtils.isString(value)) {
            
            valueObject = Json.createObjectBuilder().add(Keywords.ID, value).build();
            simpleTerm = true;

        // 9.
        } else if (JsonUtils.isObject(value)) {

            valueObject = value.asJsonObject();
            simpleTerm = false;

        } else {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 10.
        TermDefinition definition = new TermDefinition(false, protectedFlag, false);


        // 11.
        if (valueObject.containsKey(Keywords.PROTECTED)) {
            if (activeContext.inMode(Version.V1_0)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            if (JsonUtils.isNotBoolean(valueObject.get(Keywords.PROTECTED))) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROTECTED_VALUE);
            }

            definition.setProtected(valueObject.getBoolean(Keywords.PROTECTED));
        }

        // 12.
        if (valueObject.containsKey(Keywords.TYPE)) {

            // 12.1.
            JsonValue type = valueObject.get(Keywords.TYPE);

            if (JsonUtils.isNotString(type)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.2.
            String expandedTypeString = 
                        activeContext
                            .uriExpansion()                    
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(((JsonString) type).getString());

            if (expandedTypeString == null) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.3.
            if (((Keywords.JSON.equals(expandedTypeString) || Keywords.NONE.equals(expandedTypeString))
                    && activeContext.inMode(Version.V1_0))
                    // 12.4.
                    || (Keywords.noneMatch(expandedTypeString, Keywords.ID, Keywords.JSON, Keywords.NONE, Keywords.VOCAB)
                            && UriUtils.isNotAbsoluteUri(expandedTypeString))) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TYPE_MAPPING);
            }

            // 12.5.
            definition.setTypeMapping(expandedTypeString);
        }

        // 13.
        if (valueObject.containsKey(Keywords.REVERSE)) {

            // 13.1.
            if (valueObject.containsKey(Keywords.ID) || valueObject.containsKey(Keywords.NEST)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
            }

            JsonValue reverse = valueObject.get(Keywords.REVERSE);

            // 13.2.
            if (JsonUtils.isNotString(reverse)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
            }

            String reverseString = ((JsonString) reverse).getString();

            // 13.3.
            if (Keywords.matchForm(reverseString)) {
                LOGGER.log(Level.WARNING, "The value [{0}] associated with @reverse cannot have form of a keyword.", reverseString);
                return;
            }

            // 13.4.
            definition.setUriMapping( 
                        activeContext
                            .uriExpansion()
                            .localContext(localContext)
                            .defined(defined)
                            .vocab(true)
                            .expand(reverseString));

            if (UriUtils.isNotURI(definition.getUriMapping())) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
            }

            // 13.5.
            if (valueObject.containsKey(Keywords.CONTAINER)) {

                JsonValue container = valueObject.get(Keywords.CONTAINER);

                if (JsonUtils.isNotString(container) && JsonUtils.isNotNull(container)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REVERSE_PROPERTY);
                }

                if (JsonUtils.isString(container)) {

                    String containerString = ((JsonString) container).getString();

                    if (Keywords.anyMatch(containerString, Keywords.SET, Keywords.INDEX)) {
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
            return;
        }

        JsonValue idValue = valueObject.get(Keywords.ID);

        // 14.
        if (idValue != null && (JsonUtils.isNotString(idValue) || !term.equals(((JsonString) idValue).getString()))) {

            // 14.1.
            if (JsonUtils.isNotNull(idValue)) {

                // 14.2.1
                if (JsonUtils.isNotString(idValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                }

                String idValueString = ((JsonString) idValue).getString();

                // 14.2.2
                if (!Keywords.contains(idValueString) && Keywords.matchForm(idValueString)) {
                    LOGGER.log(Level.WARNING, "The value [{0}] associated with @id has form of a keyword but is not keyword.", idValueString);
                    return;
                }

                // 14.2.3
                definition.setUriMapping( 
                                activeContext
                                    .uriExpansion()
                                    .localContext(localContext)
                                    .defined(defined)
                                    .vocab(true)
                                    .expand(idValueString));

                if (Keywords.CONTEXT.equals(definition.getUriMapping())) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_ALIAS);
                }

                if (!Keywords.contains(definition.getUriMapping()) && !UriUtils.isURI(definition.getUriMapping())
                        && !BlankNode.hasPrefix(definition.getUriMapping())) {

                    throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                }

                // 14.2.4
                if (term.substring(0, term.length() - 1).indexOf(':', 1) != -1 || term.contains("/")) {

                    // 14.2.4.1
                    defined.put(term, Boolean.TRUE);

                    // 14.2.4.2
                    String expandedTerm = 
                                activeContext
                                    .uriExpansion()
                                    .localContext(localContext)
                                    .defined(defined)
                                    .vocab(true)
                                    .expand(term);

                    if (expandedTerm == null || !expandedTerm.equals(definition.getUriMapping())) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_IRI_MAPPING);
                    }
                }

                // 14.2.5
                if (!term.contains(":") && !term.contains("/") && Boolean.TRUE.equals(simpleTerm)
                        && (definition.getUriMapping() != null && ((
                            UriUtils.endsWithGenDelim(definition.getUriMapping())
                                && UriUtils.isURI(definition.getUriMapping().substring(0, definition.getUriMapping().length() - 1))
                                    )
                            || BlankNode.hasPrefix(definition.getUriMapping())))) {
                    
                    definition.setPrefix(true);
                }
            }

        // 15.
        } else if (term.indexOf(':', 1) != -1) {

            final CompactUri compactUri = CompactUri.create(term);

            // 15.1.
            if (compactUri != null && compactUri.isNotBlank() && localContext.containsKey(compactUri.getPrefix())) {

                activeContext.newTerm(localContext, defined).create(compactUri.getPrefix());
            }
            // 15.2.
            if (compactUri != null && compactUri.isNotBlank() && activeContext.containsTerm(compactUri.getPrefix())) {

                definition.setUriMapping(
                                activeContext
                                        .getTerm(compactUri.getPrefix())
                                        .map(TermDefinition::getUriMapping)
                                        .map(u -> u.concat(compactUri.getSuffix()))
                                        .orElse(null)
                                        );

            // 15.3.
            } else if (UriUtils.isURI(term) || BlankNode.hasPrefix(term)) {
                definition.setUriMapping(term);
            }

        // 16.
        } else if (term.contains("/")) {

            definition.setUriMapping( 
                            activeContext
                                .uriExpansion()
                                .localContext(localContext)
                                .defined(defined)
                                .vocab(true)
                                .expand(term));

            if (!UriUtils.isURI(definition.getUriMapping())) {
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
        if (valueObject.containsKey(Keywords.CONTAINER)) {

            // 19.1.
            JsonValue containerValue = valueObject.get(Keywords.CONTAINER);

            if (!isValidContainer(containerValue)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTAINER_MAPPING);
            }

            // 19.3.
            for (JsonValue item : JsonUtils.toJsonArray(containerValue)) {
                    definition.addContainerMapping(((JsonString)item).getString());
            }

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
        if (valueObject.containsKey(Keywords.INDEX)) {

            // 20.1.
            if (activeContext.inMode(Version.V1_0) || !definition.getContainerMapping().contains(Keywords.INDEX)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            // 20.2.
            JsonValue index = valueObject.get(Keywords.INDEX);

            if (JsonUtils.isNotString(index)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            String indexString = ((JsonString) index).getString();

            String expandedIndex =
                            activeContext
                                .uriExpansion()
                                .localContext(localContext)
                                .defined(defined)
                                .vocab(true)
                                .expand(indexString);

            if (expandedIndex == null || UriUtils.isNotURI(expandedIndex)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            definition.setIndexMapping(indexString);
        }

        // 21.
        if (valueObject.containsKey(Keywords.CONTEXT)) {

            // 21.1.
            if (activeContext.inMode(Version.V1_0)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            // 21.2.
            JsonValue context = valueObject.get(Keywords.CONTEXT);

            // 21.3.
            try {
                activeContext
                        .newContext()
                        .overrideProtected(true)
                        .remoteContexts(new ArrayList<>(remoteContexts))
                        .validateScopedContext(false)                        
                        .create(context, baseUrl);

            } catch (JsonLdError e) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_SCOPED_CONTEXT, e);
            }

            // 21.4.
            definition.setLocalContext(context);
            definition.setBaseUrl(baseUrl);
        }

        // 22.
        if (valueObject.containsKey(Keywords.LANGUAGE) && !valueObject.containsKey(Keywords.TYPE)) {

            // 22.1. - 2.
            JsonValue language = valueObject.get(Keywords.LANGUAGE);

            if (JsonUtils.isNull(language) || JsonUtils.isString(language)) {
                
                if (JsonUtils.isString(language) && !LanguageTag.isWellFormed(((JsonString)language).getString())) {
                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", ((JsonString)language).getString());
                }
                
                definition.setLanguageMapping(language);

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_MAPPING);
            }
        }

        // 23.
        if (valueObject.containsKey(Keywords.DIRECTION) && !valueObject.containsKey(Keywords.TYPE)) {

            JsonValue direction = valueObject.get(Keywords.DIRECTION);

            if (JsonUtils.isNull(direction)) {
                definition.setDirectionMapping(DirectionType.NULL);

            } else if (JsonUtils.isString(direction)) {

                String directionString = ((JsonString) direction).getString();

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
        if (valueObject.containsKey(Keywords.NEST)) {

            // 24.1
            if (activeContext.inMode(Version.V1_0)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            JsonValue nest = valueObject.get(Keywords.NEST);

            if (JsonUtils.isNotString(nest)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
            }

            String nestString = ((JsonString) nest).getString();

            if (Keywords.contains(nestString) && !Keywords.NEST.equals(nestString)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_NEST_VALUE);
            }
            definition.setNestValue(nestString);
        }

        // 25.
        if (valueObject.containsKey(Keywords.PREFIX)) {

            // 25.1.
            if (activeContext.inMode(Version.V1_0) || term.contains(":") || term.contains("/")) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
            }

            // 25.2.
            JsonValue prefix = valueObject.get(Keywords.PREFIX);

            if (JsonUtils.isTrue(prefix)) {
                definition.setPrefix(true);

            } else if (JsonUtils.isFalse(prefix) && JsonUtils.isNotNull(prefix)) {
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
        if (!Keywords.allMatch(valueObject.keySet(), Keywords.ID, Keywords.REVERSE, Keywords.CONTAINER,
                Keywords.CONTEXT, Keywords.DIRECTION, Keywords.INDEX, Keywords.LANGUAGE, Keywords.NEST, Keywords.PREFIX,
                Keywords.PROTECTED, Keywords.TYPE)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_TERM_DEFINITION);
        }

        // 27.
        if (!overrideProtectedFlag && previousDefinition.isPresent() && previousDefinition.get().isProtected()) {

            // 27.1.
            if (definition.isNotSameExcept(previousDefinition.get())) {
                throw new JsonLdError(JsonLdErrorCode.PROTECTED_TERM_REDEFINITION);
            }

            // 27.2.
            definition = previousDefinition.get();
        }

        // 28
        activeContext.setTerm(term, definition);
        defined.put(term, Boolean.TRUE);
    }
    
    private final boolean isValidContainer(JsonValue container) {
        
        if (JsonUtils.isNull(container)) {
            return false;
        }
    
        if (activeContext.inMode(Version.V1_0)) {
            
            if (JsonUtils.isNotString(container)) {
                return false;
            }

            return Keywords.noneMatch(((JsonString)container).getString(), Keywords.GRAPH, Keywords.ID, Keywords.TYPE);    
        } 
        
        if (JsonUtils.isArray(container) && container.asJsonArray().size() == 1) {
            container = container.asJsonArray().get(0);            
        }

        if (JsonUtils.isString(container) ) {

            return Keywords.anyMatch(((JsonString)container).getString(), 
                                Keywords.GRAPH, 
                                Keywords.ID, 
                                Keywords.INDEX, 
                                Keywords.LANGUAGE,
                                Keywords.LIST, 
                                Keywords.SET, 
                                Keywords.TYPE);
        }
        
        if (JsonUtils.isArray(container)) { 
            
            if (container.asJsonArray().size() > 3) {
                return false;
            }
             
            if (JsonUtils.contains(Keywords.GRAPH, container)
                    && JsonUtils.contains(Keywords.ID, container)) {

                return container.asJsonArray().size() == 2 || JsonUtils.contains(Keywords.SET, container);
            } 
            if (JsonUtils.contains(Keywords.GRAPH, container)
                    && JsonUtils.contains(Keywords.INDEX, container)) {
                
                return container.asJsonArray().size() == 2 || JsonUtils.contains(Keywords.SET, container);
            }
            
            if (container.asJsonArray().size() > 2) {
                return false;
            }

            if (JsonUtils.contains(Keywords.SET, container)) {
                
                return JsonUtils.contains(Keywords.GRAPH, container)
                        || JsonUtils.contains(Keywords.ID, container)
                        || JsonUtils.contains(Keywords.INDEX, container)
                        || JsonUtils.contains(Keywords.LANGUAGE, container)
                        || JsonUtils.contains(Keywords.TYPE, container)
                        ;
            }            
        }
        return false;
    }
    
}