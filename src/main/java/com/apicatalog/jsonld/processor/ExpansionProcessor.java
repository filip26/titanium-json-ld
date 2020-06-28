package com.apicatalog.jsonld.processor;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.expansion.Expansion;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-expand">JsonLdProcessor.expand()</a>
 *
 */
public final class ExpansionProcessor {

    ExpansionProcessor() {
    }

    public static final JsonArray expand(final URI input, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final Document remoteDocument = 
                                options
                                    .getDocumentLoader()
                                    .loadDocument(input,
                                            new DocumentLoaderOptions()
                                                    .setExtractAllScripts(options.isExtractAllScripts()));

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
        
        return expand(remoteDocument, options, false);
    }

    public static final JsonArray expand(Document input, final JsonLdOptions options, boolean frameExpansion) throws JsonLdError {

        if (input == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "RemoteDocument is null.");
        }
        
        final JsonStructure jsonStructure = input
                                                .getJsonContent()
                                                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document is not pased JSON."));
        
        // 5. Initialize a new empty active context. The base IRI and
        // original base URL of the active context is set to the documentUrl
        // from remote document, if available; otherwise to the base option from
        // options.
        // If set, the base option from options overrides the base IRI.

        URI baseUri = null;
        URI baseUrl = null;

        if (input.getDocumentUrl() != null) {
            baseUrl = input.getDocumentUrl();
            baseUri = baseUrl;                
        }

        if (baseUrl == null) {
            baseUrl = options.getBase();
        }
        if (options.getBase() != null) {
            baseUri = options.getBase();
        }
        
        ActiveContext activeContext = new ActiveContext(baseUri, baseUrl, options);

        // 6. If the expandContext option in options is set, update the active context
        // using the Context Processing algorithm, passing the expandContext as
        // local context and the original base URL from active context as base URL.
        // If expandContext is a map having an @context entry, pass that entry's value
        // instead for local context.
        if (options.getExpandContext() != null) {
            
            final Optional<JsonStructure> contextValue = options.getExpandContext().getJsonContent();

            if (contextValue.isPresent()) {
                
                final JsonArray expandedContext = JsonUtils.toJsonArray(contextValue.get());
                
                if (expandedContext.size() == 1 
                        && JsonUtils.isObject(expandedContext.get(0)) 
                        && expandedContext.getJsonObject(0).containsKey(Keywords.CONTEXT)
                        ) {
                    
                    activeContext = activeContext
                                        .newContext()
                                            .create(
                                                expandedContext.getJsonObject(0).get(Keywords.CONTEXT), 
                                                baseUrl);
                    
                } else {
                    activeContext = activeContext.newContext().create(expandedContext, baseUrl);   
                }
            }
        }
        
        // 7.
        if (input.getContextUrl() != null) {
            activeContext = activeContext
                                .newContext()
                                .create(Json.createValue(input.getContextUrl().toString()), input.getContextUrl());
        }

        // 8.
        JsonValue expanded = Expansion
                                .with(activeContext, jsonStructure, null, baseUrl)
                                .frameExpansion(frameExpansion)
                                .ordered(options.isOrdered())
                                .compute();

        // 8.1
        if (JsonUtils.isObject(expanded)) {

            final JsonObject object = expanded.asJsonObject();

            if (object.size() == 1 && object.containsKey(Keywords.GRAPH)) {
                expanded = object.get(Keywords.GRAPH);
            }
        }

        // 8.2
        if (JsonUtils.isNull(expanded)) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }

        // 8.3
        return JsonUtils.toJsonArray(expanded);
    }
}
