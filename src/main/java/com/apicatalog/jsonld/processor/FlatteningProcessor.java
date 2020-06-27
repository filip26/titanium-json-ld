package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.flattening.Flattening;
import com.apicatalog.jsonld.http.media.MediaType;
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
            return flatten(input, (Document)null, options);
        }

        final Document contextDocument = options.getDocumentLoader().loadDocument(context, new LoadDocumentOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }
                
        return flatten(input, contextDocument, options);        
    }

    public static final JsonStructure flatten(final Document input, final URI context, final JsonLdOptions options) throws JsonLdError {
        
        if (context == null) {
            return flatten(input, (Document)null, options);
        }
                
        final Document contextDocument = options.getDocumentLoader().loadDocument(context, new LoadDocumentOptions());

        if (contextDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.INVALID_REMOTE_CONTEXT, "Context[" + context + "] is null.");
        }
                
        return flatten(input, contextDocument, options);        
    }

    public static final JsonStructure flatten(final URI input, final Document context, final JsonLdOptions options) throws JsonLdError {
        
        if (options.getDocumentLoader() == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final Document remoteDocument = 
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

    public static final JsonStructure flatten(final Document input, final Document context, final JsonLdOptions options) throws JsonLdError {
        
        // 4.
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);
        
        final JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);
        
        // 5.
        // 6.
        JsonStructure flattenedOutput = Flattening.with(expandedInput).ordered(options.isOrdered()).flatten();

        // 6.1.
        if (context != null) {
         
            final Document document = JsonDocument.of(MediaType.JSON_LD, flattenedOutput);
            
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
