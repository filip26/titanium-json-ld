package com.apicatalog.jsonld.json;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.RemoteContent;
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
                
        if (jsonContent.getContent().isRawPayload()) {
            
            byte[] payload = jsonContent.getContent().getRawPayload()
                                .orElseThrow(() -> error(contentUri, "RemoteDocument.Document.getRawPayload() is empty"));
            
            jsonContent.setContent(RemoteContent.parseJson(new ByteArrayInputStream(payload)));    
        }

        return jsonContent;
    }
    
    public static final JsonStructure extractJsonStructure(RemoteDocument document) throws JsonLdError {
        
        if (document == null) {
            throw new IllegalArgumentException("Document paramater is null.");
        }

        if (document.getContent() == null) {
            throw new IllegalArgumentException("Document content is null.");
        }

        final Optional<JsonStructure> contentStructure;
        
        if (document.getContent().isRawPayload()) {
            
            byte[] payload = document.getContent().getRawPayload()
                                .orElseThrow(() -> error(document.getDocumentUrl(), "RemoteDocument.Document.getRawPayload() is empty"));
            
            contentStructure = RemoteContent.parseJson(new ByteArrayInputStream(payload)).getJsonStructure();
                        
        } else {
            contentStructure = document.getContent().getJsonStructure();    
        }

        return contentStructure.orElseThrow(() -> error(document.getDocumentUrl(), ""));        
    }

    private final RemoteDocument fetch(final URI contentUri, final LoadDocumentOptions options) throws JsonLdError {
        RemoteDocument content = loader.loadDocument(contentUri, options);
        
        if (content == null) {
            throw error(contentUri, "null has been returned");
        }
        
        if (content.getContent() == null) {
            throw error(contentUri, "RemoteDocument.Document is null");
        }
        
        return content;
    }
    
    private static final JsonLdError error(final URI contentUri, final String details) {
        return new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Cannot get [" + contentUri + "], ".concat(details).concat("."));
    }
}
