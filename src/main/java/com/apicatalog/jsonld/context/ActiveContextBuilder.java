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
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.uri.UriResolver;
import com.apicatalog.jsonld.uri.UriUtils;

/**
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#context-processing-algorithms">Context
 *      Processing Algorithm</a>
 * 
 */
public final class ActiveContextBuilder {

    private static final int MAX_REMOTE_CONTEXTS = 256; 
    
    private static final Logger LOGGER = Logger.getLogger(ActiveContextBuilder.class.getName());
    
    // mandatory
    private final ActiveContext activeContext;

    // optional
    private Collection<String> remoteContexts;

    private boolean overrideProtected;

    private boolean propagate;

    private boolean validateScopedContext;

    // runtime
    private ActiveContext result;

    private ActiveContextBuilder(final ActiveContext activeContext) {

        this.activeContext = activeContext;

        // default optional values
        this.remoteContexts = new ArrayList<>();
        this.overrideProtected = false;
        this.propagate = true;
        this.validateScopedContext = true;
        
        // runtime
        this.result = null;
    }

    public static final ActiveContextBuilder with(final ActiveContext activeContext) {        
        return new ActiveContextBuilder(activeContext);
    }

    public ActiveContextBuilder remoteContexts(Collection<String> value) {
        this.remoteContexts = value;
        return this;
    }

    public ActiveContextBuilder overrideProtected(boolean value) {
        this.overrideProtected = value;
        return this;
    }

    public ActiveContextBuilder propagate(boolean value) {
        this.propagate = value;
        return this;
    }

    public ActiveContextBuilder validateScopedContext(boolean value) {
        this.validateScopedContext = value;
        return this;
    }

    public ActiveContext create(final JsonValue localContext, final URI baseUrl) throws JsonLdError {

        // 1. Initialize result to the result of cloning active context, with inverse
        // context set to null.
        result = new ActiveContext(activeContext);
        result.setInverseContext(null);

        // 2. If local context is an object containing the member @propagate,
        // its value MUST be boolean true or false, set propagate to that value.
        if (JsonUtils.isObject(localContext)) {

            final JsonObject localContextObject = localContext.asJsonObject();
            
            if (localContextObject.containsKey(Keywords.PROPAGATE)) {
                
                final JsonValue propagateValue = localContextObject.get(Keywords.PROPAGATE);
                
                if (JsonUtils.isNotBoolean(propagateValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
                }
                
                propagate = JsonUtils.isTrue(propagateValue);
            }
        }

        // 3. If propagate is false, and result does not have a previous context,
        // set previous context in result to active context.
        if (!propagate && result.getPreviousContext() == null) {
            result.setPreviousContext(activeContext);
        }

        // 4. If local context is not an array, set local context to an array containing
        // only local context.
        // 5. For each item context in local context:
        for (JsonValue itemContext : JsonUtils.toJsonArray(localContext)) {

            // 5.1. If context is null:
            if (JsonUtils.isNull(itemContext)) {

                // 5.1.1. If override protected is false and active context contains any
                // protected term definitions,
                // an invalid context nullification has been detected and processing is aborted.
                if (!overrideProtected && result.containsProtectedTerm()) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_NULLIFICATION);
                }

                // 5.1.2. Initialize result as a newly-initialized active context,
                // setting both base IRI and original base URL to the value of original base URL
                // in active context,
                // and, if propagate is false, previous context in result to the previous value
                // of result.
                result = propagate
                        ? new ActiveContext(activeContext.getBaseUrl(), activeContext.getBaseUrl(), activeContext.getOptions())
                        : new ActiveContext(activeContext.getBaseUrl(), activeContext.getBaseUrl(), result.getPreviousContext(),
                                activeContext.getOptions());

                // 5.1.3. Continue with the next context
                continue;
            }

            // 5.2. if context is a string,
            if (JsonUtils.isString(itemContext)) {

                fetch(((JsonString)itemContext).getString(), baseUrl);

                // 5.2.7
                continue;
            }

            // 5.3. If context is not a map, an invalid local context error has been
            // detected and processing is aborted.
            if (JsonUtils.isNotObject(itemContext)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LOCAL_CONTEXT);
            }

            // 5.4. Otherwise, context is a context definition
            JsonObject contextDefinition = itemContext.asJsonObject();

            // 5.5. If context has an @version
            if (contextDefinition.containsKey(Keywords.VERSION)) {

                final JsonValue version = contextDefinition.get(Keywords.VERSION);

                String versionString = null;

                if (JsonUtils.isString(version)) {
                    versionString = ((JsonString) version).getString();

                } else if (JsonUtils.isNumber(version)) {
                    versionString = version.toString();
                }

                // 5.5.1. If the associated value is not 1.1, an invalid @version value has been
                // detected, and processing is aborted.
                if (!"1.1".equals(versionString)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_VERSION_VALUE);
                }

                // 5.5.2.
                if (activeContext.inMode(Version.V1_0)) {
                    throw new JsonLdError(JsonLdErrorCode.PROCESSING_MODE_CONFLICT);
                }
            }

            // 5.6. If context has an @import
            if (contextDefinition.containsKey(Keywords.IMPORT)) {
                
                // 5.6.1.
                if (activeContext.inMode(Version.V1_0)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }

                final JsonValue contextImport = contextDefinition.get(Keywords.IMPORT);

                // 5.6.2.
                if (JsonUtils.isNotString(contextImport)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE);
                }

                // 5.6.3.
                final String contextImportUri = UriResolver.resolve(baseUrl, ((JsonString) contextImport).getString());

                // 5.6.4.
                if (activeContext.getOptions().getDocumentLoader() == null) {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED);
                }

                final DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
                loaderOptions.setProfile(ProfileConstants.CONTEXT);
                loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

                JsonStructure importedStructure = null;

                try {

                    final Document importedDocument = activeContext.getOptions().getDocumentLoader().loadDocument(URI.create(contextImportUri), loaderOptions);

                    if (importedDocument == null) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context[" + contextImportUri + "] is null.");
                    }

                    importedStructure = importedDocument
                                            .getJsonContent()
                                            .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE));
                    
                // 5.6.5
                } catch (JsonLdError e) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE, e);
                }

                // 5.6.6
                if (JsonUtils.isNotObject(importedStructure)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT);
                }

                JsonObject importedContext = importedStructure.asJsonObject();

                if (!importedContext.containsKey(Keywords.CONTEXT)
                        || JsonUtils.isNotObject(importedContext.get(Keywords.CONTEXT))) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT);
                }

                importedContext = importedContext.getJsonObject(Keywords.CONTEXT);

                // 5.6.7
                if (importedContext.containsKey(Keywords.IMPORT)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.6.8
                contextDefinition = JsonUtils.merge(importedContext, contextDefinition);
            }

            // 5.7. If context has an @base entry and remote contexts is empty,
            // i.e., the currently being processed context is not a remote context:
            if (contextDefinition.containsKey(Keywords.BASE) /*&& remoteContexts.isEmpty()*/) {
                // 5.7.1
                JsonValue value = contextDefinition.get(Keywords.BASE);

                // 5.7.2.
                if (JsonUtils.isNull(value)) {
                    result.setBaseUri(null);

                } else {

                    if (JsonUtils.isNotString(value)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI);
                    }

                    String valueString = ((JsonString) value).getString();
                    
                    if (UriUtils.isURI(valueString)) {

                        // 5.7.3
                        if (UriUtils.isAbsoluteUri(valueString)) {
                            result.setBaseUri(URI.create(valueString));

                        // 5.7.4
                        } else if (result.getBaseUri() != null) {

                            String resolved = UriResolver.resolve(result.getBaseUri(), valueString);
                            
                            result.setBaseUri(UriUtils.create(resolved));
                            
                        } else {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI);
                        }       
                        
                    } else if (!valueString.isBlank()) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI);
                    }
                }
            }

            // 5.8.
            if (contextDefinition.containsKey(Keywords.VOCAB)) {
                
                // 5.8.1.
                final JsonValue value = contextDefinition.get(Keywords.VOCAB);

                // 5.8.2.
                if (JsonUtils.isNull(value)) {
                    result.setVocabularyMapping(null);

                // 5.8.3
                } else if (JsonUtils.isNotString(value)) {
                    
                    throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING);
                        
                } else {

                    final String valueString = ((JsonString) value).getString();

                    if (valueString.isBlank() || BlankNode.hasPrefix(valueString) || UriUtils.isURI(valueString)) {

                        String vocabularyMapping =
                                    result
                                        .uriExpansion()
                                        .vocab(true)
                                        .documentRelative(true)
                                        .expand(valueString);

                        if (BlankNode.hasPrefix(valueString) || UriUtils.isURI(vocabularyMapping)) {
                            result.setVocabularyMapping(vocabularyMapping);

                        } else {
                            throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING);
                        }

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING);
                    }
                }
            }

            // 5.9.
            if (contextDefinition.containsKey(Keywords.LANGUAGE)) {
                
                // 5.9.1
                final JsonValue value = contextDefinition.get(Keywords.LANGUAGE);

                // 5.9.2.
                if (JsonUtils.isNull(value)) {
                    result.setDefaultLanguage(null);

                // 5.9.3
                } else {

                    if (JsonUtils.isNotString(value)) {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_DEFAULT_LANGUAGE);
                    }

                    result.setDefaultLanguage(((JsonString)value).getString());
                    
                    if (!LanguageTag.isWellFormed(result.getDefaultLanguage())) {
                        LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", result.getDefaultLanguage());                        
                    }
                }
            }

            // 5.10.
            if (contextDefinition.containsKey(Keywords.DIRECTION)) {

                // 5.10.1.
                if (activeContext.inMode(Version.V1_0)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.10.2.
                final JsonValue value = contextDefinition.get(Keywords.DIRECTION);

                // 5.10.3.
                if (JsonUtils.isNull(value)) {
                    result.setDefaultBaseDirection(DirectionType.NULL);

                // 5.10.4.
                } else if (JsonUtils.isNotString(value)) {
                    
                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    
                } else {

                    final String direction = ((JsonString) value).getString();

                    if ("ltr".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(DirectionType.LTR);

                    } else if ("rtl".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(DirectionType.RTL);

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    }
                }
            }

            // 5.11.
            if (contextDefinition.containsKey(Keywords.PROPAGATE)) {
                // 5.11.1.
                if (activeContext.inMode(Version.V1_0)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }
                // 5.11.2.
                if (JsonUtils.isNotBoolean(contextDefinition.get(Keywords.PROPAGATE))) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
                }
            }

            final TermDefinitionBuilder termBuilder =
                            result
                                .newTerm(contextDefinition, new HashMap<>())
                                .baseUrl(baseUrl)
                                .overrideProtectedFlag(overrideProtected);

            // 5.13
            for (final String key : contextDefinition.keySet()) {

                if (Keywords.noneMatch(key, Keywords.BASE, Keywords.DIRECTION, Keywords.IMPORT, Keywords.LANGUAGE,
                        Keywords.PROPAGATE, Keywords.PROTECTED, Keywords.VERSION, Keywords.VOCAB)) {

                    termBuilder.protectedFlag(
                                    contextDefinition.containsKey(Keywords.PROTECTED)
                                        && JsonUtils.isTrue(contextDefinition.get(Keywords.PROTECTED))
                                        )
                                .remoteContexts(new ArrayList<>(remoteContexts))
                                .create(key);
                }
            }
        }
        // 6.
        return result;
    }
    
    private void fetch(final String context, final URI baseUrl) throws JsonLdError {

        String contextUri = context;
        
        // 5.2.1
        if (baseUrl != null) {
            contextUri = UriResolver.resolve(baseUrl, contextUri);
            
        } else if (UriUtils.isNotURI(contextUri)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not URI [" + contextUri + "].");
        }

        if (UriUtils.isNotAbsoluteUri(contextUri)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not absolute [" + contextUri + "].");
        }

        // 5.2.2
        if (!validateScopedContext && remoteContexts.contains(contextUri)) {
            return;
        }

        // 5.2.3
        if (remoteContexts.size() > MAX_REMOTE_CONTEXTS) {
            throw new JsonLdError(JsonLdErrorCode.CONTEXT_OVERFLOW, "Too many contexts [>" + MAX_REMOTE_CONTEXTS + "].");
        }
        
        remoteContexts.add(contextUri);

        // 5.2.5.
        if (activeContext.getOptions().getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Document loader is null. Cannot fetch [" + contextUri + "].");
        }

        DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
        loaderOptions.setProfile(ProfileConstants.CONTEXT);
        loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

        JsonStructure importedStructure = null;
        URI documentUrl = null;

        try {
            
            final Document remoteImport = activeContext.getOptions().getDocumentLoader().loadDocument(URI.create(contextUri), loaderOptions);

            if (remoteImport == null) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.");
            }

            documentUrl = remoteImport.getDocumentUrl();

            importedStructure = remoteImport.getJsonContent()
                                    .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.")); 
                                
        // 5.2.5.1.
        } catch (JsonLdError e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
        }

        // 5.2.5.2.
        if (JsonUtils.isNotObject(importedStructure)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is not valid Json Object [" + importedStructure.getValueType() + "].");
        }

        JsonValue importedContext = importedStructure.asJsonObject();

        if (!importedContext.asJsonObject().containsKey(Keywords.CONTEXT)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context does not contain @context key and is not valid JSON-LD context.");
        }

        // 5.2.5.3.
        importedContext = importedContext.asJsonObject().get(Keywords.CONTEXT);

        // remote @base from a remote context
        if (JsonUtils.isObject(importedContext) && importedContext.asJsonObject().containsKey(Keywords.BASE)) {
            importedContext = Json.createObjectBuilder(importedContext.asJsonObject()).remove(Keywords.BASE).build();
        }
        
        // 5.2.6
        try {
            result = result
                        .newContext()
                        .remoteContexts(new ArrayList<>(remoteContexts))
                        .validateScopedContext(validateScopedContext)
                        .create(importedContext, documentUrl);

        } catch (JsonLdError e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
        }
    }
}
