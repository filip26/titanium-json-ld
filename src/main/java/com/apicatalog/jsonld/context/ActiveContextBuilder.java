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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.StringUtils;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.uri.UriResolver;
import com.apicatalog.jsonld.uri.UriUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

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

    public CompletableFuture<ActiveContext> create(final JsonValue localContext, final URI baseUrl) {

        // 1. Initialize result to the result of cloning active context, with inverse
        // context set to null.
        result = new ActiveContext(activeContext);
        result.setInverseContext(null);

        // 2. If local context is an object containing the member @propagate,
        // its value MUST be boolean true or false, set propagate to that value.
        if (JsonUtils.isObject(localContext)) {

            final JsonValue propagateValue = localContext.asJsonObject()
                    .get(Keywords.PROPAGATE);

            if (propagateValue != null) {

                if (JsonUtils.isNotBoolean(propagateValue)) {
                    return CompletableFuture.failedFuture(new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE));
                }

                propagate = JsonUtils.isTrue(propagateValue);
            }
        }

        // 3. If propagate is false, and result does not have a previous context,
        // set previous context in result to active context.
        if (!propagate && result.getPreviousContext() == null) {
            result.setPreviousContext(activeContext);
        }

        CompletableFuture<ActiveContext> next = CompletableFuture.completedFuture(result);

        // 4. If local context is not an array, set local context to an array containing
        // only local context.
        // 5. For each item context in local context:
        for (final JsonValue itemContext : JsonUtils.toCollection(localContext)) {
            next = next.thenCompose(result -> {
                try {
                    return context(localContext, baseUrl, result, itemContext);
                } catch (JsonLdError e) {
                    return CompletableFuture.failedFuture(e);
                }
            });
            if (next.isCompletedExceptionally()) {
                return next;
            }
        }

        // 6.
        return next;
    }

    private CompletableFuture<ActiveContext> context(final JsonValue localContext, final URI baseUrl, final ActiveContext activeContext, final JsonValue itemContext) throws JsonLdError {

        // 5.1. If context is null:
        if (JsonUtils.isNull(itemContext)) {

            // 5.1.1. If override protected is false and active context contains any
            // protected term definitions,
            // an invalid context nullification has been detected and processing is aborted.
            if (!overrideProtected && activeContext.containsProtectedTerm()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_NULLIFICATION);
            }

            // 5.1.2. Initialize result as a newly-initialized active context,
            // setting both base IRI and original base URL to the value of original base URL
            // in active context,
            // and, if propagate is false, previous context in result to the previous value
            // of result.
            return CompletableFuture.completedFuture(propagate
                    ? new ActiveContext(activeContext.getBaseUrl(), activeContext.getBaseUrl(), activeContext.runtime())
                    : new ActiveContext(activeContext.getBaseUrl(), activeContext.getBaseUrl(), result.getPreviousContext(), activeContext.runtime()));
        }

        // 5.2. if context is a string,
        if (JsonUtils.isString(itemContext)) {

            URI contextUri;

            try {
                contextUri = URI.create(((JsonString) itemContext).getString());

                // 5.2.1
                if (baseUrl != null) {
                    contextUri = UriResolver.resolveAsUri(baseUrl, contextUri);
                }

                if (!contextUri.isAbsolute()) {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not absolute [" + contextUri + "].");
                }

            } catch (IllegalArgumentException e) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not URI [" + itemContext + "].");
            }

            final String contextKey = contextUri.toString();

            // 5.2.2
            if (validateScopedContext || !remoteContexts.contains(contextKey)) {
                return fetch(contextUri, contextKey, baseUrl);
            }

            // 5.2.7
            return CompletableFuture.completedFuture(activeContext);
        }

        // 5.3. If context is not a map, an invalid local context error has been
        // detected and processing is aborted.
        if (JsonUtils.isNotObject(itemContext)) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_LOCAL_CONTEXT);
        }

        // 5.4. Otherwise, context is a context definition
        JsonObject itemContextDefinition = itemContext.asJsonObject();

        final JsonValue version = itemContextDefinition.get(Keywords.VERSION);

        // 5.5. If context has an @version
        if (version != null) {

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
            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.PROCESSING_MODE_CONFLICT);
            }
        }

        // 5.6. If context has an @import
        if (itemContextDefinition.containsKey(Keywords.IMPORT)) {

            CompletableFuture<JsonObject> next = CompletableFuture.completedFuture(itemContextDefinition);

            // 5.6.1.
            if (activeContext.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
            }

            final JsonValue contextImport = itemContextDefinition.get(Keywords.IMPORT);

            // 5.6.2.
            if (JsonUtils.isNotString(contextImport)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE, "Invalid context @import value [" + contextImport + "].");
            }

            // 5.6.3.
            final URI contextImportUri = UriResolver.resolveAsUri(baseUrl, ((JsonString) contextImport).getString());

            // 5.6.4.
            if (activeContext.runtime().getDocumentLoader() == null) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED);
            }

            final DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
            loaderOptions.setProfile(ProfileConstants.CONTEXT);
            loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

            next = next.thenCompose(
                    contextDefinition -> {
                        return activeContext.runtime().getDocumentLoader().loadDocument(contextImportUri, loaderOptions)
                                .thenCompose(importedDocument -> {

                                    JsonStructure importedStructure;
                                    try {

                                        if (importedDocument == null) {

                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context[" + contextImportUri + "] is null.");
                                        }

                                        importedStructure = importedDocument
                                                .getJsonContent()
                                                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE));

                                        // 5.6.6
                                        if (JsonUtils.isNotObject(importedStructure)) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT);
                                        }

                                        final JsonValue importedContext = importedStructure.asJsonObject().get(Keywords.CONTEXT);

                                        if (importedContext == null
                                                || JsonUtils.isNotObject(importedContext)) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT);
                                        }

                                        final JsonObject importedContextObject = importedContext.asJsonObject();

                                        // 5.6.7
                                        if (importedContextObject.containsKey(Keywords.IMPORT)) {
                                            throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                                        }

                                        // 5.6.8
                                        return CompletableFuture.completedFuture(JsonUtils.merge(importedContextObject, contextDefinition));

                                    } catch (JsonLdError e) {
                                        return CompletableFuture.failedFuture(e);
                                    }

                                });
                    });
            return next.thenCompose(contextDefinition -> completedFuture(activeContext, contextDefinition, localContext, baseUrl));
        }

        return completedFuture(activeContext, itemContextDefinition, localContext, baseUrl);
    }

    private CompletableFuture<ActiveContext> completedFuture(ActiveContext context, JsonObject contextDefinition, JsonValue localContext, URI baseUrl) {
        try {
            return CompletableFuture.completedFuture(complete(context, contextDefinition, localContext, baseUrl));
        } catch (JsonLdError e) {
            return CompletableFuture.failedFuture(e);
        }
    }

    private ActiveContext complete(ActiveContext context, JsonObject contextDefinition, JsonValue localContext, URI baseUrl) throws JsonLdError {
        // 5.7. If context has an @base entry and remote contexts is empty,
        // i.e., the currently being processed context is not a remote context:
        if (contextDefinition.containsKey(Keywords.BASE) /* && remoteContexts.isEmpty() */) {
            // 5.7.1
            JsonValue value = contextDefinition.get(Keywords.BASE);

            // 5.7.2.
            if (JsonUtils.isNull(value)) {
                context.setBaseUri(null);

            } else if (JsonUtils.isString(value)) {

                final String valueString = ((JsonString) value).getString();

                final URI valueUri = StringUtils.isNotBlank(valueString) ? UriUtils.create(valueString) : null;

                if (valueUri != null) {

                    // 5.7.3
                    if (valueUri.isAbsolute()) {
                        context.setBaseUri(valueUri);

                        // 5.7.4
                    } else if (context.getBaseUri() != null) {
                        context.setBaseUri(UriResolver.resolveAsUri(context.getBaseUri(), valueUri));

                    } else {
                        LOGGER.log(Level.FINE,
                                "5.7.4: valueString={0}, localContext={1}, baseUrl={2}",
                                new Object[] { valueString, localContext, baseUrl });

                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI,
                                "A relative base IRI cannot be resolved [@base = " + valueString +
                                        "]. Please use JsonLdOptions.setBase() method to set an absolute IRI.");
                    }

                } else if (StringUtils.isNotBlank(valueString)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI,
                            "An invalid base IRI has been detected [@base = " + valueString + "].");
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_IRI,
                        "An invalid base IRI has been detected [@base = " + value + "].");
            }
        }

        // 5.8.
        if (contextDefinition.containsKey(Keywords.VOCAB)) {

            // 5.8.1.
            final JsonValue value = contextDefinition.get(Keywords.VOCAB);

            // 5.8.2.
            if (JsonUtils.isNull(value)) {
                context.setVocabularyMapping(null);

                // 5.8.3
            } else if (JsonUtils.isString(value)) {

                final String valueString = ((JsonString) value).getString();

                if (StringUtils.isBlank(valueString) || BlankNode.hasPrefix(valueString) || UriUtils.isURI(valueString)) {

                    final String vocabularyMapping = context
                            .uriExpansion()
                            .vocab(true)
                            .documentRelative(true)
                            .expand(valueString);

                    if (BlankNode.hasPrefix(valueString) || UriUtils.isURI(vocabularyMapping)) {
                        context.setVocabularyMapping(vocabularyMapping);

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING, "An invalid vocabulary mapping [" + vocabularyMapping + "] has been detected.");
                    }

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING, "An invalid vocabulary mapping [" + valueString + "] has been detected.");
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_VOCAB_MAPPING);
            }
        }

        // 5.9.
        if (contextDefinition.containsKey(Keywords.LANGUAGE)) {

            // 5.9.1
            final JsonValue value = contextDefinition.get(Keywords.LANGUAGE);

            // 5.9.2.
            if (JsonUtils.isNull(value)) {
                context.setDefaultLanguage(null);

                // 5.9.3
            } else if (JsonUtils.isString(value)) {

                context.setDefaultLanguage(((JsonString) value).getString());

                if (!LanguageTag.isWellFormed(context.getDefaultLanguage())) {
                    LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", context.getDefaultLanguage());
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_DEFAULT_LANGUAGE);
            }
        }

        // 5.10.
        if (contextDefinition.containsKey(Keywords.DIRECTION)) {

            // 5.10.1.
            if (context.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
            }

            // 5.10.2.
            final JsonValue value = contextDefinition.get(Keywords.DIRECTION);

            // 5.10.3.
            if (JsonUtils.isNull(value)) {
                context.setDefaultBaseDirection(DirectionType.NULL);

                // 5.10.4.
            } else if (JsonUtils.isString(value)) {

                final String direction = ((JsonString) value).getString();

                if ("ltr".equalsIgnoreCase(direction)) {
                    context.setDefaultBaseDirection(DirectionType.LTR);

                } else if ("rtl".equalsIgnoreCase(direction)) {
                    context.setDefaultBaseDirection(DirectionType.RTL);

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                }

            } else {
                throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
            }
        }

        // 5.11.
        if (contextDefinition.containsKey(Keywords.PROPAGATE)) {
            // 5.11.1.
            if (context.runtime().isV10()) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
            }
            // 5.11.2.
            if (JsonUtils.isNotBoolean(contextDefinition.get(Keywords.PROPAGATE))) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
            }
        }

        final TermDefinitionBuilder termBuilder = context
                .newTerm(contextDefinition, new HashMap<>())
                .baseUrl(baseUrl)
                .overrideProtectedFlag(overrideProtected);

        // 5.13
        for (final String key : contextDefinition.keySet()) {

            if (Keywords.noneMatch(key, Keywords.BASE, Keywords.DIRECTION, Keywords.IMPORT, Keywords.LANGUAGE,
                    Keywords.PROPAGATE, Keywords.PROTECTED, Keywords.VERSION, Keywords.VOCAB)) {

                termBuilder
                        .protectedFlag(JsonUtils.isTrue(contextDefinition.get(Keywords.PROTECTED)))
                        .remoteContexts(new ArrayList<>(remoteContexts))
                        .create(key);
            }
        }
        return context;
    }

    private CompletableFuture<ActiveContext> fetch(final URI contextUri, final String contextKey, final URI baseUrl) throws JsonLdError {

        // 5.2.3
        if (remoteContexts.size() > MAX_REMOTE_CONTEXTS) {
            throw new JsonLdError(JsonLdErrorCode.CONTEXT_OVERFLOW, "Too many contexts [>" + MAX_REMOTE_CONTEXTS + "].");
        }

        remoteContexts.add(contextKey);

        // 5.2.4
        if (activeContext.runtime().getContextCache() != null
                && activeContext.runtime().getContextCache().containsKey(contextKey) && !validateScopedContext) {

            JsonValue cachedContext = activeContext.runtime().getContextCache().get(contextKey);
            return result
                    .newContext()
                    .remoteContexts(new ArrayList<>(remoteContexts))
                    .validateScopedContext(validateScopedContext)
                    .create(cachedContext, contextUri);
        }

        // 5.2.5.
        if (activeContext.runtime().getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Document loader is null. Cannot fetch [" + contextUri + "].");
        }

        Document remoteImport = null;

        if (activeContext.runtime().getDocumentCache() != null
                && activeContext.runtime().getDocumentCache().containsKey(contextKey)) {

            remoteImport = activeContext.runtime().getDocumentCache().get(contextKey);
        }

        if (remoteImport == null) {

            DocumentLoaderOptions loaderOptions = new DocumentLoaderOptions();
            loaderOptions.setProfile(ProfileConstants.CONTEXT);
            loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

            try {

                remoteImport = activeContext.runtime().getDocumentLoader().loadDocument(contextUri, loaderOptions).get();

                // 5.2.5.1.
            } catch (InterruptedException | ExecutionException e) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "There was a problem encountered loading a remote context [" + contextUri + "]", e);
            }

            if (remoteImport == null) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.");
            }
        }

        final JsonStructure importedStructure = remoteImport.getJsonContent()
                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null."));

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
        if (JsonUtils.containsKey(importedContext, Keywords.BASE)) {
            importedContext = JsonProvider.instance().createObjectBuilder(importedContext.asJsonObject()).remove(Keywords.BASE).build();
        }

        if (activeContext.runtime().getDocumentCache() != null) {
            activeContext.runtime().getDocumentCache().put(contextKey, remoteImport);
        }

        // 5.2.6
//        try {
        return result
                .newContext()
                .remoteContexts(new ArrayList<>(remoteContexts))
                .validateScopedContext(validateScopedContext)
                .create(importedContext, remoteImport.getDocumentUrl())
//                    .ha
//                    .exceptionally(e -> CompletableFuture.failedFuture(new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e)))
        ;

//FIXME            if (result.runtime().getContextCache() != null && !validateScopedContext) {
//                result.runtime().getContextCache().put(contextKey, importedContext);
//            }

//FIXME        } catch (JsonLdError e) {
//            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
//        }
    }
}
