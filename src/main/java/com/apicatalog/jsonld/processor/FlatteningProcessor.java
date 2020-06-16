package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.FlatteningBuilder;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

/**
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#dom-jsonldprocessor-compact">JsonLdProcessor.compact()</a>
 *
 */
public final class FlatteningProcessor {

    private FlatteningProcessor() {
    }
    
    public static final JsonStructure flatten(final URI input, final URI context, final JsonLdOptions options) throws JsonLdError {
        
        if (context == null) {
            return flatten(input, (JsonStructure)null, options);
        }
        
        RemoteDocument jsonContext = options.getDocumentLoader().loadDocument(context, new LoadDocumentOptions());
        
        return flatten(input, jsonContext.getDocument().asJsonStructure(), options);        
    }
    
    public static final JsonStructure flatten(final URI input, final JsonStructure context, final JsonLdOptions options) throws JsonLdError {
        
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
        
        return flatten(remoteDocument, context, options);
    }

    public static final JsonStructure flatten(final RemoteDocument input, final JsonStructure context, final JsonLdOptions options) throws JsonLdError {
        
        // 4.
        JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);
        
        JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);
        
        // 5.
        
        // 6.
        JsonStructure flattenedOutput = FlatteningBuilder.with(expandedInput).ordered(options.isOrdered()).build();

        // 6.1.
        if (JsonUtils.isNotNull(context)) {
         
            RemoteDocument document = new RemoteDocument();
            document.setDocument(JsonDocument.of(flattenedOutput));
            
            JsonLdOptions compactionOptions = new JsonLdOptions(options);
            
            if (options.getBase() != null) {
                compactionOptions.setBase(options.getBase());
                
            } else if (options.isCompactArrays()) {
                compactionOptions.setBase(input.getDocumentUrl());
            }
            
            flattenedOutput = CompactionProcessor.compact(document, context, compactionOptions);
        }
        
        return flattenedOutput;            
    }
}
