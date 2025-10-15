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
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.api.StringUtils;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.jsonld.json.JsonProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.DirectionType;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.jsonld.node.BlankNode;
import com.apicatalog.jsonld.uri.UriResolver;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;

import jakarta.json.JsonObject;
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

    public ActiveContext create(final Object localContext, final NodeAdapter adapter, final URI baseUrl) throws JsonLdError, IOException {

        // 1. Initialize result to the result of cloning active context, with inverse
        // context set to null.
        result = new ActiveContext(activeContext);
        result.setInverseContext(null);

        // 2. If local context is an object containing the member @propagate,
        // its value MUST be boolean true or false, set propagate to that value.
        if (adapter.isMap(localContext)) {

            var propagateValue = adapter.property(Keywords.PROPAGATE, localContext);

            if (!adapter.isNull(propagateValue)) {

                if (!adapter.isBoolean(propagateValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
                }

                propagate = adapter.type(propagateValue) == NodeType.TRUE;
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
        for (var itemContext : adapter.asIterable(localContext)) {

            // 5.1. If context is null:
            if (adapter.isNull(itemContext)) {

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
                        ? new ActiveContext(activeContext.getBaseUrl(),
                                activeContext.getBaseUrl(),
                                activeContext.runtime())
                                
                        : new ActiveContext(activeContext.getBaseUrl(),
                                activeContext.getBaseUrl(),
                                result.getPreviousContext(),
                                activeContext.runtime());

                // 5.1.3. Continue with the next context
                continue;
            }

            // 5.2. if context is a string,
            if (adapter.isString(itemContext)) {
                fetch(adapter.stringValue(itemContext), baseUrl);
                continue;
            }

            // 5.3. If context is not a map, an invalid local context error has been
            // detected and processing is aborted.
            if (!adapter.isMap(itemContext)) {
                throw new JsonLdError(JsonLdErrorCode.INVALID_LOCAL_CONTEXT);
            }

            // 5.4. Otherwise, it's a context definition
            var version = adapter.property(Keywords.VERSION, itemContext);

            // 5.5. If context has an @version
            if (version != null) {

                String versionString = null;

                if (adapter.isString(version)) {
                    versionString = adapter.stringValue(version);

                } else if (adapter.isNumber(version)) {
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
            var contextImport = adapter.property(Keywords.IMPORT, itemContext);

            var contextDefinition = itemContext;

            if (contextImport != null) {

                // 5.6.1.
                if (activeContext.runtime().isV10()) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.6.2.
                if (!adapter.isString(contextImport)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE, "Invalid context @import value [" + contextImport + "].");
                }

                // 5.6.3.
                final URI contextImportUri = UriResolver.resolveAsUri(
                        baseUrl,
                        adapter.stringValue(contextImport));

                // 5.6.4.
                if (activeContext.runtime().getDocumentLoader() == null) {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED);
                }

                final LoaderOptions loaderOptions = new LoaderOptions();
                loaderOptions.setProfile(ProfileConstants.CONTEXT);
                loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

                JsonStructure importedStructure = null;

                try {

                    final Document importedDocument = activeContext.runtime().getDocumentLoader().loadDocument(contextImportUri, loaderOptions);

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
//               FIXME importedContextObject.forEach(contextDefinition::put);
                contextDefinition = JsonUtils.merge(importedContextObject, (JsonObject) contextDefinition);
            }

            // 5.7. If context has an @base entry and remote contexts is empty,
            // i.e., the currently being processed context is not a remote context:

            var baseValue = adapter.property(Keywords.BASE, contextDefinition);

            if (baseValue != null /* && remoteContexts.isEmpty() */) {
                // 5.7.1

                // 5.7.2.
                if (adapter.isNull(baseValue)) {
                    result.setBaseUri(null);

                } else if (adapter.isString(baseValue)) {

                    final String valueString = adapter.stringValue(baseValue);

                    final URI valueUri = valueString != null && !valueString.isBlank() ? UriUtils.create(valueString) : null;

                    if (valueUri != null) {

                        // 5.7.3
                        if (valueUri.isAbsolute()) {
                            result.setBaseUri(valueUri);

                            // 5.7.4
                        } else if (result.getBaseUri() != null) {
                            result.setBaseUri(UriResolver.resolveAsUri(result.getBaseUri(), valueUri));

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
                            "An invalid base IRI has been detected [@base = " + baseValue + "].");
                }
            }

            // 5.8.
            var vocabValue = adapter.property(Keywords.VOCAB, contextDefinition);
            System.out.println(">>> " + contextDefinition );
            if (vocabValue != null) {

                // 5.8.2.
                if (adapter.isNull(vocabValue)) {
                    result.setVocabularyMapping(null);

                    // 5.8.3
                } else if (adapter.isString(vocabValue)) {

                    final String valueString = adapter.stringValue(vocabValue);

                    if (StringUtils.isBlank(valueString) || BlankNode.hasPrefix(valueString) || UriUtils.isURI(valueString)) {

                        final String vocabularyMapping = result
                                .uriExpansion()
                                .vocab(true)
                                .documentRelative(true)
                                .expand(valueString);
                        System.out.println(">> " + contextDefinition);
                        System.out.println(">> " + valueString + " - > " + vocabularyMapping);
                        if (BlankNode.hasPrefix(valueString) || UriUtils.isURI(vocabularyMapping)) {
                            result.setVocabularyMapping(vocabularyMapping);

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
            var langValue = adapter.property(Keywords.LANGUAGE, contextDefinition);

            if (langValue != null) {

                // 5.9.2.
                if (adapter.isNull(langValue)) {
                    result.setDefaultLanguage(null);

                    // 5.9.3
                } else if (adapter.isString(langValue)) {

                    result.setDefaultLanguage(adapter.stringValue(langValue));

                    if (!LanguageTag.isWellFormed(result.getDefaultLanguage())) {
                        LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", result.getDefaultLanguage());
                    }

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_DEFAULT_LANGUAGE);
                }
            }

            // 5.10.
            var dirValue = adapter.property(Keywords.DIRECTION, contextDefinition);

            if (dirValue != null) {

                // 5.10.1.
                if (activeContext.runtime().isV10()) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.10.3.
                if (adapter.isNull(dirValue)) {
                    result.setDefaultBaseDirection(DirectionType.NULL);

                    // 5.10.4.
                } else if (adapter.isString(dirValue)) {

                    final String direction = adapter.stringValue(dirValue);

                    if ("ltr".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(DirectionType.LTR);

                    } else if ("rtl".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(DirectionType.RTL);

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                    }

                } else {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                }
            }

            // 5.11.
            var propagateValue = adapter.property(Keywords.PROPAGATE, contextDefinition);

            if (propagateValue != null) {
                // 5.11.1.
                if (activeContext.runtime().isV10()) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_CONTEXT_ENTRY);
                }
                // 5.11.2.
                if (!adapter.isBoolean(propagateValue)) {
                    throw new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
                }
            }

            final TermDefinitionBuilder termBuilder = result
                    .newTerm(contextDefinition, adapter, new HashMap<>())
                    .baseUrl(baseUrl)
                    .overrideProtectedFlag(overrideProtected);

            // 5.13
            for (var keyValue : adapter.keys(contextDefinition)) {

                if (keyValue instanceof String key
                        && Keywords.noneMatch((String) key,
                                Keywords.BASE, Keywords.DIRECTION, Keywords.IMPORT, Keywords.LANGUAGE,
                                Keywords.PROPAGATE, Keywords.PROTECTED, Keywords.VERSION, Keywords.VOCAB)) {

                    termBuilder
                            .protectedFlag(adapter.isTrue(adapter.property(Keywords.PROTECTED, contextDefinition)))
                            .remoteContexts(new ArrayList<>(remoteContexts))
                            .create(key);
                }
            }
        }
        // 6.
        return result;
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

    private void fetch(final String context, final URI baseUrl) throws JsonLdError, IOException {

        URI contextUri;

        try {
            contextUri = URI.create(context);

            // 5.2.1
            if (baseUrl != null) {
                contextUri = UriResolver.resolveAsUri(baseUrl, contextUri);
            }

            if (!contextUri.isAbsolute()) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not absolute [" + contextUri + "].");
            }

        } catch (IllegalArgumentException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, "Context URI is not URI [" + context + "].");
        }

        final String contextKey = contextUri.toString();

        // 5.2.2
        if (!validateScopedContext && remoteContexts.contains(contextKey)) {
            return;
        }

        // 5.2.3
        if (remoteContexts.size() > MAX_REMOTE_CONTEXTS) {
            throw new JsonLdError(JsonLdErrorCode.CONTEXT_OVERFLOW, "Too many contexts [>" + MAX_REMOTE_CONTEXTS + "].");
        }

        remoteContexts.add(contextKey);

        // 5.2.4
        if (activeContext.runtime().getContextCache() != null
                && activeContext.runtime().getContextCache().containsKey(contextKey) && !validateScopedContext) {

            JsonValue cachedContext = activeContext.runtime().getContextCache().get(contextKey);
            result = result
                    .newContext()
                    .remoteContexts(new ArrayList<>(remoteContexts))
                    .validateScopedContext(validateScopedContext)
                    // FIXME adapter
                    .create(cachedContext, JakartaAdapter.instance(), contextUri);
            return;
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

            LoaderOptions loaderOptions = new LoaderOptions();
            loaderOptions.setProfile(ProfileConstants.CONTEXT);
            loaderOptions.setRequestProfile(Arrays.asList(loaderOptions.getProfile()));

            try {

                remoteImport = activeContext.runtime().getDocumentLoader().loadDocument(contextUri, loaderOptions);

                // 5.2.5.1.
            } catch (JsonLdError e) {
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
        try {
            result = result
                    .newContext()
                    .remoteContexts(new ArrayList<>(remoteContexts))
                    .validateScopedContext(validateScopedContext)
                    // FIXME - adapter
                    .create(importedContext, JakartaAdapter.instance(), remoteImport.getDocumentUrl());

            if (result.runtime().getContextCache() != null && !validateScopedContext) {
                result.runtime().getContextCache().put(contextKey, importedContext);
            }

        } catch (JsonLdError e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
        }
    }
}
