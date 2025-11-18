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
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLd.Version;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.expansion.UriExpansion;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.processor.ExecutionEvents;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.tree.io.java.NativeMaterializer;
import com.apicatalog.web.lang.LanguageTag;
import com.apicatalog.web.uri.UriResolver;
import com.apicatalog.web.uri.UriUtils;

/**
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#context-processing-algorithms">Context
 *      Processing Algorithm</a>
 *
 */
public final class ContextBuilder {

    private static final int MAX_REMOTE_CONTEXTS = 256;

    private static final Logger LOGGER = Logger.getLogger(ContextBuilder.class.getName());

    // mandatory
    private final ActiveContext activeContext;
    private final DocumentLoader loader;
    private final ExecutionEvents runtime;

    // optional
    private Collection<String> remoteContexts;

    private boolean overrideProtected;

    private boolean propagate;

    private boolean validateScopedContext;
    
    private boolean acceptInlineContext;
    
    private Consumer<Collection<String>> collectKeys;

    // runtime
    private ActiveContext result;

    private ContextBuilder(final ActiveContext activeContext, final DocumentLoader loader, final ExecutionEvents runtime) {

        this.activeContext = activeContext;
        this.loader = loader;
        this.runtime = runtime;

        // default optional values
        this.remoteContexts = new ArrayList<>();
        this.overrideProtected = false;
        this.propagate = true;
        this.validateScopedContext = true;
        this.acceptInlineContext = true;
        this.collectKeys = null;

        // runtime
        this.result = null;
    }

    public static final ContextBuilder with(
            final ActiveContext activeContext,
            final DocumentLoader loader,
            final ExecutionEvents runtime) {
        return new ContextBuilder(activeContext, loader, runtime);
    }

    public ActiveContext build(
            final TreeIO context,
            final URI baseUrl) throws JsonLdException {
        return build(context.node(), context.adapter(), baseUrl);
    }

    public ActiveContext build(
            final Object contextValue,
            final TreeAdapter adapter,
            final URI baseUrl) throws JsonLdException {

        // 1. Initialize result to the result of cloning active context, with inverse
        // context set to null.
        result = new ActiveContext(activeContext);
        result.setInverseContext(null);
        // TODO better
//        result.setVersion(activeContext.runtime().version());

        // 2. If local context is an object containing the member @propagate,
        // its value MUST be boolean true or false, set propagate to that value.
        if (adapter.isMap(contextValue)) {

            if (!acceptInlineContext) {
                throw new JsonLdException(ErrorCode.INLINE_CONTEXT_IS_NOT_ALLOWED);
            }
            
            var propagateValue = adapter.property(Keywords.PROPAGATE, contextValue);

            if (!adapter.isNull(propagateValue)) {

                if (!adapter.isBoolean(propagateValue)) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE);
                }

                propagate = adapter.isTrue(propagateValue);
            }
        }

        // 3. If propagate is false, and result does not have a previous context, set
        // previous context in result to active context.
        if (!propagate && result.getPreviousContext() == null) {
            result.setPreviousContext(activeContext);
        }

        // 5. For each item context in local context:
        for (var itemContext : adapter.asIterable(contextValue)) {

            // 5.1. If context is null:
            if (adapter.isNull(itemContext)) {

                // 5.1.1. If override protected is false and active context contains any
                // protected term definitions, an invalid context nullification has been
                // detected and processing is aborted.
                if (!overrideProtected && result.containsProtectedTerm()) {
                    throw new JsonLdException(ErrorCode.INVALID_CONTEXT_NULLIFICATION);
                }

                // 5.1.2. Initialize result as a newly-initialized active context, setting both
                // base IRI and original base URL to the value of original base URL in active
                // context, and, if propagate is false, previous context in result to the
                // previous value of result.
                result = propagate
                        ? new ActiveContext(activeContext.getBaseUrl(),
                                activeContext.getBaseUrl(),
                                activeContext.version())

                        : new ActiveContext(activeContext.getBaseUrl(),
                                activeContext.getBaseUrl(),
                                result.getPreviousContext(),
                                activeContext.version());

                // 5.1.3. Continue with the next context
                continue;
            }

            // 5.2. if context is a string,
            if (adapter.isString(itemContext)) {
                fetch(adapter.stringValue(itemContext), baseUrl);
                continue;
            }
            
            if (!acceptInlineContext) {
                throw new JsonLdException(ErrorCode.INLINE_CONTEXT_IS_NOT_ALLOWED); 
            }

            // 5.3. If context is not a map, an invalid local context error has been
            // detected and processing is aborted.
            if (!adapter.isMap(itemContext)) {
                throw new JsonLdException(ErrorCode.INVALID_LOCAL_CONTEXT, "A context must be a map, context=" + itemContext);
            }

            if (collectKeys != null) {
                collectKeys.accept(adapter.keyStream(itemContext).map(adapter::asString).toList());
            }
            
            // 5.4. Otherwise, it's a context definition
            var versionNode = adapter.property(Keywords.VERSION, itemContext);

            // 5.5. If context has an @version
            if (versionNode != null) {

                String versionString = null;

                if (adapter.isString(versionNode)) {
                    versionString = adapter.stringValue(versionNode);

                } else if (adapter.isNumber(versionNode)) {
                    versionString = versionNode.toString();
                }

                // 5.5.1. If the associated value is not 1.1, an invalid @version value has been
                // detected, and processing is aborted.
                if (!"1.1".equals(versionString)) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_VERSION_VALUE);
                }

                // 5.5.2.
                if (activeContext.isV10()) {
                    throw new JsonLdException(ErrorCode.PROCESSING_MODE_CONFLICT);
                }

                result.setVersion(Version.V1_1);
            }

            // 5.6. If context has an @import
            var contextImport = adapter.property(Keywords.IMPORT, itemContext);

            var contextDefinition = itemContext;
            var contextAdapter = adapter;

            if (contextImport != null) {

                // 5.6.1.
                if (activeContext.isV10()) {
                    throw new JsonLdException(ErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.6.2.
                if (!adapter.isString(contextImport)) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_IMPORT_VALUE, "Invalid context @import value [" + contextImport + "].");
                }

                // 5.6.3.
                final var contextImportUri = UriResolver.resolveAsUri(
                        baseUrl,
                        adapter.stringValue(contextImport));

                // 5.6.4.
                if (loader == null) {
                    throw new JsonLdException(ErrorCode.MISSING_DOCUMENT_LOADER, "Remote context cannot be loaded, a loader is not present, uri = " + contextImportUri);
                }

                final var loaderOptions = new DocumentLoader.Options(
                        false,
                        Terms.PROFILE_CONTEXT,
                        List.of(Terms.PROFILE_CONTEXT));

                TreeIO importedContent = null;

                try {

                    final Document importedDocument = loader.loadDocument(contextImportUri, loaderOptions);

                    if (importedDocument == null) {
                        throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null, uri = " + contextImportUri);
                    }

//                    if (importedDocument instanceof JsonDocument jsonDocument) {
//                    if (importedDocument.content() instanceof PolyNode adaptedContent) {
                    importedContent = importedDocument.content();
//                    } else {
//                        throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Invalid context " + contextImportUri + " to import.");
//                    }
//                    importedStructure = importedDocument
//                            .getJsonContent()
//                            .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.INVALID_KEYWORD_IMPORT_VALUE));

                    // 5.6.5
                } catch (JsonLdException e) {
                    throw new JsonLdException(ErrorCode.INVALID_KEYWORD_IMPORT_VALUE, e);
                }

                var importedNode = importedContent.node();
                var importAdapter = importedContent.adapter();

                // 5.6.6
                if (!importAdapter.isMap(importedNode)) {
                    throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT);
                }

                var importedContext = importAdapter.property(Keywords.CONTEXT, importedNode);

                if (importedContext == null
                        || !importAdapter.isMap(importedContext)) {
                    throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT);
                }

                // 5.6.7
                if (importAdapter.keys(importedContext).contains(Keywords.IMPORT)) {
                    throw new JsonLdException(ErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.6.8
                final Map<Object, Object> merged;

                try {

                    if (importAdapter.isCompatibleWith(NativeAdapter.instance())
                            && importedContext instanceof Map<?, ?> map) {
                        merged = new LinkedHashMap<>(map);

                    } else {
                        merged = new LinkedHashMap<>((Map<?, ?>) NativeMaterializer.node(importedContext, importAdapter));
                    }

                    if (importAdapter.isCompatibleWith(NativeAdapter.instance())
                            && contextDefinition instanceof Map<?, ?> map) {
                        merged.putAll(map);

                    } else {
                        merged.putAll((Map<?, ?>) NativeMaterializer.node(contextDefinition, adapter));
                    }

                } catch (TreeIOException e) {
                    throw new JsonLdException(ErrorCode.UNSPECIFIED, e);
                }

                contextAdapter = NativeAdapter.instance();
                contextDefinition = merged;
            }

            // 5.7. If context has an @base entry and remote contexts is empty,
            // i.e., the currently being processed context is not a remote context:
            var baseValue = contextAdapter.property(Keywords.BASE, contextDefinition);

            if (baseValue != null /* && remoteContexts.isEmpty() */) {

                // 5.7.2.
                if (contextAdapter.isNull(baseValue)) {
                    result.setBaseUri(null);

                } else if (contextAdapter.isString(baseValue)) {

                    final var valueString = contextAdapter.stringValue(baseValue);

                    final var valueUri = valueString != null && !valueString.isBlank()
                            ? UriUtils.create(valueString)
                            : null;

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
                                    new Object[] { valueString, contextValue, baseUrl });

                            throw new JsonLdException(ErrorCode.INVALID_BASE_IRI,
                                    "A relative base IRI cannot be resolved, @base=" + valueString +
                                            ". Use Options.setBase() method to set an absolute IRI.");
                        }

                    } else if (valueString != null && !valueString.isBlank()) {
                        throw new JsonLdException(ErrorCode.INVALID_BASE_IRI,
                                "An invalid base IRI has been detected, @base=" + valueString);
                    }

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_BASE_IRI,
                            "An invalid base IRI has been detected, @base=" + baseValue);
                }
            }

            // 5.8.
            var vocabValue = contextAdapter.property(Keywords.VOCAB, contextDefinition);

            if (vocabValue != null) {

                // 5.8.2.
                if (contextAdapter.isNull(vocabValue)) {
                    result.setVocabularyMapping(null);

                    // 5.8.3
                } else if (contextAdapter.isString(vocabValue)) {

                    final var valueString = contextAdapter.stringValue(vocabValue);

                    if (valueString == null
                            || valueString.isBlank()
                            || BlankNode.hasPrefix(valueString)
                            || UriUtils.isURI(valueString)) {

                        final var vocabularyMapping = UriExpansion.with(result, loader, runtime)
                                .vocab(true)
                                .documentRelative(true)
                                .expand(valueString);

                        if (BlankNode.hasPrefix(valueString) || UriUtils.isURI(vocabularyMapping)) {
                            result.setVocabularyMapping(vocabularyMapping);

                        } else {
                            throw new JsonLdException(ErrorCode.INVALID_VOCAB_MAPPING, "An invalid vocabulary mapping has been detected, @vocab=" + vocabularyMapping);
                        }

                    } else {
                        throw new JsonLdException(ErrorCode.INVALID_VOCAB_MAPPING, "An invalid vocabulary mapping has been detected, @vocab=" + valueString);
                    }

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_VOCAB_MAPPING, "A vocabulary mapping is not string value, @vocab=" + vocabValue);
                }
            }

            // 5.9.
            var langValue = contextAdapter.property(Keywords.LANGUAGE, contextDefinition);

            if (langValue != null) {

                // 5.9.2.
                if (contextAdapter.isNull(langValue)) {
                    result.setDefaultLanguage(null);

                    // 5.9.3
                } else if (contextAdapter.isString(langValue)) {

                    result.setDefaultLanguage(contextAdapter.stringValue(langValue));

                    if (!LanguageTag.isWellFormed(result.getDefaultLanguage())) {
                        LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed.", result.getDefaultLanguage());
                    }

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_DEFAULT_LANGUAGE, "Language tag is not string value, @language=" + langValue);
                }
            }

            // 5.10.
            final var dirValue = contextAdapter.property(Keywords.DIRECTION, contextDefinition);

            if (dirValue != null) {

                // 5.10.1.
                if (activeContext.isV10()) {
                    throw new JsonLdException(ErrorCode.INVALID_CONTEXT_ENTRY);
                }

                // 5.10.3.
                if (contextAdapter.isNull(dirValue)) {
                    result.setDefaultBaseDirection(Direction.NULL);

                    // 5.10.4.
                } else if (contextAdapter.isString(dirValue)) {

                    final String direction = contextAdapter.stringValue(dirValue);

                    if ("ltr".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(Direction.LTR);

                    } else if ("rtl".equalsIgnoreCase(direction)) {
                        result.setDefaultBaseDirection(Direction.RTL);

                    } else {
                        throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
                    }

                } else {
                    throw new JsonLdException(ErrorCode.INVALID_BASE_DIRECTION);
                }
            }

            // 5.11.
            var propagateValue = contextAdapter.property(Keywords.PROPAGATE, contextDefinition);

            if (propagateValue != null) {
                // 5.11.1.
                if (activeContext.isV10()) {
                    throw new JsonLdException(ErrorCode.INVALID_CONTEXT_ENTRY);
                }
                // 5.11.2.
                if (!contextAdapter.isBoolean(propagateValue)) {
                    throw new JsonLdException(
                            ErrorCode.INVALID_KEYWORD_PROPAGATE_VALUE,
                            "Expected boolean but got %s".formatted(propagateValue));
                }
            }

            final var termBuilder = TermDefinitionBuilder.with(result, contextDefinition, contextAdapter, new HashMap<>(), loader, runtime)
                    .baseUrl(baseUrl)
                    .overrideProtectedFlag(overrideProtected);

            // 5.13
            for (var keyValue : contextAdapter.keys(contextDefinition)) {

                if (keyValue instanceof String key
                        && Keywords.noneMatch((String) key,
                                Keywords.BASE, Keywords.DIRECTION, Keywords.IMPORT, Keywords.LANGUAGE,
                                Keywords.PROPAGATE, Keywords.PROTECTED, Keywords.VERSION, Keywords.VOCAB)) {

                    termBuilder
                            .protectedFlag(contextAdapter.isTrue(contextAdapter.property(Keywords.PROTECTED, contextDefinition)))
                            .remoteContexts(new ArrayList<>(remoteContexts))
                            .create(key);
                }
            }
        }
        // 6.
        return result;
    }

    public ContextBuilder remoteContexts(Collection<String> value) {
        this.remoteContexts = value;
        return this;
    }

    public ContextBuilder overrideProtected(boolean value) {
        this.overrideProtected = value;
        return this;
    }

    public ContextBuilder propagate(boolean value) {
        this.propagate = value;
        return this;
    }

    public ContextBuilder validateScopedContext(boolean value) {
        this.validateScopedContext = value;
        return this;
    }
    
    public ContextBuilder acceptInlineContext(boolean accept) {
        this.acceptInlineContext = accept;
        return this;
    }
    
    public ContextBuilder collectKeys(Consumer<Collection<String>> collectKeys) {
        this.collectKeys = collectKeys;
        return this;
    }

    private void fetch(final String uri, final URI baseUrl) throws JsonLdException {

        URI contextUri;

        try {
            contextUri = URI.create(uri);

            // 5.2.1
            if (baseUrl != null) {
                contextUri = UriResolver.resolveAsUri(baseUrl, contextUri);
            }

            if (!contextUri.isAbsolute()) {
                throw new JsonLdException(ErrorCode.LOADING_REMOTE_CONTEXT_FAILED,
                        "Context URI is not absolute [" + contextUri + "].");
            }

        } catch (IllegalArgumentException e) {
            throw new JsonLdException(ErrorCode.LOADING_REMOTE_CONTEXT_FAILED,
                    "Context URI is not URI [" + uri + "].");
        }

        final String contextKey = contextUri.toString();

        // 5.2.2
        if (!validateScopedContext && remoteContexts.contains(contextKey)) {
            return;
        }

        // 5.2.3
        if (remoteContexts.size() > MAX_REMOTE_CONTEXTS) {
            throw new JsonLdException(ErrorCode.CONTEXT_OVERFLOW,
                    "Too many contexts, limit=" + MAX_REMOTE_CONTEXTS);
        }

        remoteContexts.add(contextKey);

        // FIXME
//        if (activeContext.runtime().getContextCache() != null
//                && activeContext.runtime().getContextCache().containsKey(contextKey) && !validateScopedContext) {
//
//            var cachedContext = activeContext.runtime().getContextCache().get(contextKey);
//            result = result
//                    .newContext()
//                    .remoteContexts(new ArrayList<>(remoteContexts))
//                    .validateScopedContext(validateScopedContext)
//                    // FIXME adapter
//                    .build(cachedContext, JakartaAdapter.instance(), contextUri);
//            return;
//        }

        // 5.2.5.
        if (loader == null) {
            throw new JsonLdException(ErrorCode.MISSING_DOCUMENT_LOADER,
                    "Document loader is not present, uri=" + contextUri);
        }

        Document remoteDocument = null;

        // FIXME
//        if (activeContext.runtime().getDocumentCache() != null
//                && activeContext.runtime().getDocumentCache().containsKey(contextKey)) {
//
//            remoteImport = activeContext.runtime().getDocumentCache().get(contextKey);
//        }

        if (remoteDocument == null) {

            final var loaderOptions = new DocumentLoader.Options(
                    false,
                    Terms.PROFILE_CONTEXT,
                    List.of(Terms.PROFILE_CONTEXT));

            try {

                remoteDocument = loader.loadDocument(contextUri, loaderOptions);

                // 5.2.5.1.
            } catch (JsonLdException e) {
                throw new JsonLdException(ErrorCode.LOADING_REMOTE_CONTEXT_FAILED,
                        "There was a problem encountered loading a remote context, uri=" + contextUri, e);
            }

            if (remoteDocument == null) {
                throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT,
                        "Imported context is null.");
            }
        }

        final TreeIO importedContent = remoteDocument.content();

//        if (remoteImport.content() instanceof PolyNode adaptedNode) {
//            importedNode = adaptedNode;
//
////        } else {
//        if (importedNode ==)
//            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Imported context is null.");
//        }

        // 5.2.5.2.
        if (!TreeIO.isMap(importedContent)) {
            throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT,
                    "An invalid imported JSON-LD, uri=" + contextUri + ", @context=" + importedContent.node());
        }

        // 5.2.5.3.
        final var importedContext = importedContent.property(Keywords.CONTEXT);

        if (importedContext == null) {
            throw new JsonLdException(ErrorCode.INVALID_REMOTE_CONTEXT,
                    "Imported context does not contain @context key and is not valid JSON-LD context.");
        }

        var newContext = importedContext;
        var newContextAdapter = importedContent.adapter();

        if (newContextAdapter.isMap(newContext)
                && newContextAdapter.keys(newContext).contains(Keywords.BASE)) {

            try {
                newContext = NativeMaterializer.node(importedContext, importedContent.adapter());
                newContextAdapter = NativeAdapter.instance();

                // remove @base from a remote context
                if (newContext instanceof Map map && map.containsKey(Keywords.BASE)) {
                    @SuppressWarnings("unchecked")
                    var hashMap = new LinkedHashMap<>(map);
                    hashMap.remove(Keywords.BASE);
                    newContext = hashMap;
                }
            } catch (TreeIOException e) {
                throw new JsonLdException(ErrorCode.UNSPECIFIED, e);
            }
        }

        // FIXME
//        if (activeContext.runtime().getDocumentCache() != null) {
//            activeContext.runtime().getDocumentCache().put(contextKey, remoteImport);
//        }

//        if (collectKey != null) {
//            collectKey.accept(newContextAdapter.keyStream(newContext).map(newContextAdapter::asString).toList());
//        }
        
        // 5.2.6
        try {
            result = result
                    .newContext(loader, runtime)
                    .remoteContexts(new ArrayList<>(remoteContexts))
                    .validateScopedContext(validateScopedContext)
                    .collectKeys(collectKeys)
                    .build(newContext, newContextAdapter, remoteDocument.url());

//FIXME
//            if (result.runtime().getContextCache() != null && !validateScopedContext) {
//                result.runtime().getContextCache().put(contextKey, importedContext);
//            }

        } catch (JsonLdException e) {
            throw new JsonLdException(ErrorCode.LOADING_REMOTE_CONTEXT_FAILED, e);
        }
    }
}
