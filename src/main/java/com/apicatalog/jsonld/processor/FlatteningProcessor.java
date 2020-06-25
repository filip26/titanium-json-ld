package com.apicatalog.jsonld.processor;

import java.net.URI;

import javax.json.JsonArray;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.flattening.Flattening;
import com.apicatalog.jsonld.json.JsonContentProvider;
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
            return flatten(input, (RemoteDocument)null, options);
        }
        
        
        final RemoteDocument contextDocument = JsonContentProvider
                                                .create(options.getDocumentLoader())
                                                .fetchJsonDocument(context, new LoadDocumentOptions());
                
        return flatten(input, contextDocument, options);        
    }

    public static final JsonStructure flatten(final RemoteDocument input, final URI context, final JsonLdOptions options) throws JsonLdError {
        
        if (context == null) {
            return flatten(input, (RemoteDocument)null, options);
        }
        
        
        final RemoteDocument contextDocument = JsonContentProvider
                                                .create(options.getDocumentLoader())
                                                .fetchJsonDocument(context, new LoadDocumentOptions());
                
        return flatten(input, contextDocument, options);        
    }

    public static final JsonStructure flatten(final URI input, final RemoteDocument context, final JsonLdOptions options) throws JsonLdError {
        
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

    public static final JsonStructure flatten(final RemoteDocument input, final RemoteDocument context, final JsonLdOptions options) throws JsonLdError {
        
        // 4.
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);
        expansionOptions.setOrdered(false);
        
        final JsonArray expandedInput = ExpansionProcessor.expand(input, expansionOptions, false);
        
        // 5.
        // 6.
        JsonStructure flattenedOutput = Flattening.with(expandedInput).ordered(options.isOrdered()).flatten();

        // 6.1.
        if (context != null) {
         
            RemoteDocument document = new RemoteDocument(null);
            document.setContent(RemoteContent.of(flattenedOutput));
            
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
