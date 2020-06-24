package com.apicatalog.jsonld.json;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public final class JsonContentProvider {
    
    private final LoadDocumentCallback loader;
    
    private JsonContentProvider(final LoadDocumentCallback loader) {
        this.loader = loader;
    }
    
    public static final JsonContentProvider create(final LoadDocumentCallback loader) {
        if (loader == null) {
            throw new IllegalArgumentException("Document loader is null.");
        }
        return new JsonContentProvider(loader);
    }
    
    public final JsonStructure fetchJsonStructure(URI contentUri, LoadDocumentOptions options) throws JsonLdError {
        return extractJsonStructure(fetch(contentUri, options));
    }

    public RemoteDocument fetchJsonDocument(final URI contentUri, final LoadDocumentOptions options) throws JsonLdError {
        
        final RemoteDocument jsonContent = fetch(contentUri, options);
                
        if (jsonContent.getDocument().isRawPayload()) {
            
            byte[] payload = jsonContent.getDocument().getRawPayload()
                                .orElseThrow(() -> error(contentUri, "RemoteDocument.Document.getRawPayload() is empty"));
            
            jsonContent.setDocument(JsonDocument.parse(new ByteArrayInputStream(payload)));    
        }

        return jsonContent;
    }
    
    public static final JsonStructure extractJsonStructure(RemoteDocument document) throws JsonLdError {
        
        final Optional<JsonStructure> contentStructure;
        
        if (document.getDocument().isRawPayload()) {
            
            byte[] payload = document.getDocument().getRawPayload()
                                .orElseThrow(() -> error(document.getDocumentUrl(), "RemoteDocument.Document.getRawPayload() is empty"));
            
            contentStructure = JsonDocument.parse(new ByteArrayInputStream(payload)).getJsonStructure();
                        
        } else {
            contentStructure = document.getDocument().getJsonStructure();    
        }

        return contentStructure.orElseThrow(() -> error(document.getDocumentUrl(), ""));        
    }

    private final RemoteDocument fetch(final URI contentUri, final LoadDocumentOptions options) throws JsonLdError {
        RemoteDocument content = loader.loadDocument(contentUri, options);
        
        if (content == null) {
            throw error(contentUri, "null has been returned");
        }
        
        if (content.getDocument() == null) {
            throw error(contentUri, "RemoteDocument.Document is null");
        }
        
        return content;
    }
    
    private static final JsonLdError error(final URI contentUri, final String details) {
        return new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Cannot get [" + contentUri + "], ".concat(details).concat("."));
    }
}
