package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.compaction.CompactionBuilder;
import com.apicatalog.jsonld.context.ActiveContext;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.grammar.Keywords;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.utils.JsonUtils;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class CompactionProcessor {

    CompactionProcessor() {
    }

    public static final JsonObject compact(final URI input, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        
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

    public static final JsonObject compact(final RemoteDocument input, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        
        // 4.
        JsonLdOptions expansionOptions = new JsonLdOptions();
        expansionOptions.setOrdered(false);
        expansionOptions.setExtractAllScripts(false);
        
        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions);
        
        // 5.
        URI contextBase = input.getDocumentUrl();
        
        if (contextBase == null) {
            contextBase = options.getBase();
        }
        
        // 6.
        JsonValue contextValue = context.asJsonArray();
        
        if (contextValue.asJsonArray().size() == 1) {
            contextValue = contextValue.asJsonArray().get(0);
        }
        
        if (JsonUtils.isObject(contextValue) && contextValue.asJsonObject().containsKey(Keywords.CONTEXT)) {
            contextValue = contextValue.asJsonObject().get(Keywords.CONTEXT);
        }
        
        // 7.
        ActiveContext activeContext = new ActiveContext(null, null, options);
        
        activeContext = activeContext.create(contextValue, contextBase).build();
        
        // 8.
        if (options.getBase() != null) {
            activeContext.setBaseUri(options.getBase());
            
        } else if (options.isCompactToRelative()) {
            activeContext.setBaseUri(input.getDocumentUrl());
            
        } else {
            activeContext.setBaseUri(null);
        }
        
        // 9.
        JsonValue compactedOutput = CompactionBuilder
                                        .with(activeContext, null, expandedInput)
                                        .compactArrays(options.isCompactArrays())
                                        .ordered(options.isOrdered())
                                        .build();
        
        // 9.1.
        if (JsonUtils.isEmptyArray(compactedOutput)) {
            compactedOutput = JsonValue.EMPTY_JSON_OBJECT;
            
        // 9.2.
        } else if (JsonUtils.isArray(contextValue)) {
            compactedOutput = Json.createObjectBuilder()
                                    .add(
                                        activeContext.compacttUri(Keywords.GRAPH).build(),
                                        compactedOutput
                                        )
                                    .build();
        }
        
        // 9.3.
        if (JsonUtils.isNotNull(contextValue)) {
//            compactedOutput = Json.createObjectBuilder(compactedOutput.asJsonObject())
//                                    .add(Keywords.CONTEXT, contextValue)
//                                    .build();
//FIXME            
        }

        if (JsonUtils.isObject(compactedOutput)) {
            return compactedOutput.asJsonObject();            
        }
        
        return null;    //FIXME
    }
}
