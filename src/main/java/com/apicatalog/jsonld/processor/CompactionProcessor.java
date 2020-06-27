package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.compaction.Compaction;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.json.JsonContentProvider;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class CompactionProcessor {

    private CompactionProcessor() {
    }
    
    public static final JsonObject compact(final URI input, final URI context, final JsonLdOptions options) throws JsonLdError {
        return compact(input, 
                        JsonContentProvider
                                .create(options.getDocumentLoader())
                                .fetch(context, new LoadDocumentOptions()), options);
    }
    
    public static final JsonObject compact(final URI input, final RemoteDocument context, final JsonLdOptions options) throws JsonLdError {

        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final RemoteDocument remoteDocument = 
                                options
                                    .getDocumentLoader()
                                    .loadDocument(input,
                                            new LoadDocumentOptions()
                                                    .setExtractAllScripts(options.isExtractAllScripts()));

        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
        
        return compact(remoteDocument, context, options);
    }

    public static final JsonObject compact(final RemoteDocument input, final RemoteDocument context, final JsonLdOptions options) throws JsonLdError {
        
        // 4.
        JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);
        expansionOptions.setExtractAllScripts(false);
        
        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);

        // 5.
        URI contextBase = input.getDocumentUrl();
        
        if (contextBase == null) {
            contextBase = options.getBase();
        }
        
        // 6.
        JsonValue contextValue = context.getJsonContent().orElse(JsonValue.EMPTY_JSON_OBJECT);
        
        if (JsonUtils.isArray(contextValue) && contextValue.asJsonArray().size() == 1) {
            contextValue = contextValue.asJsonArray().get(0);
        }
        
        if (JsonUtils.isObject(contextValue) && contextValue.asJsonObject().containsKey(Keywords.CONTEXT)) {
            contextValue = contextValue.asJsonObject().get(Keywords.CONTEXT);
        }
        
        // 7.
        ActiveContext activeContext = new ActiveContext(options);
        
        activeContext = activeContext.newContext().create(contextValue, contextBase);

        // 8.
        if (options.getBase() != null) {
            activeContext.setBaseUri(options.getBase());
            
        } else if (activeContext.getBaseUri() == null && options.isCompactToRelative()) {
            activeContext.setBaseUri(input.getDocumentUrl());            
        }
        
        // 9.
        JsonValue compactedOutput = Compaction
                                        .with(activeContext)
                                        .compactArrays(options.isCompactArrays())
                                        .ordered(options.isOrdered())
                                        .compact(expandedInput);
        
        // 9.1.
        if (JsonUtils.isEmptyArray(compactedOutput)) {
            compactedOutput = JsonValue.EMPTY_JSON_OBJECT;
            
        // 9.2.
        } else if (JsonUtils.isArray(compactedOutput)) {
            compactedOutput = Json.createObjectBuilder()
                                    .add(
                                        activeContext.uriCompaction().vocab(true).compact(Keywords.GRAPH),
                                        compactedOutput
                                        )
                                    .build();
        }

        if (JsonUtils.isNull(compactedOutput) || compactedOutput.asJsonObject().isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        }
        
        // 9.3.
        if (JsonUtils.isNotNull(contextValue) && JsonUtils.isNotEmptyArray(contextValue) && JsonUtils.isNotEmptyObject(contextValue)) {
            compactedOutput = Json.createObjectBuilder(compactedOutput.asJsonObject())
                                    .add(Keywords.CONTEXT, contextValue)
                                    .build();            
        }

        return compactedOutput.asJsonObject();            
    }
}
