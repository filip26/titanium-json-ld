package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.io.Reader;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.http.media.MediaType;

final class DocumentParser {

    private DocumentParser() {
    }
    
    public static Document parse(final MediaType contentType, final InputStream inputStream)  throws JsonLdError {
        
        if (inputStream == null) {
            throw new IllegalArgumentException("The provided content InputStream is null.");
        }
        
        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }

        if (JsonDocument.accepts(contentType)) {       
            return JsonDocument.of(contentType, inputStream);
        }
        
        if (RdfDocument.accepts(contentType)) {
            return RdfDocument.of(contentType, inputStream);
        }
        
        return fireUnsupportedMediaType(contentType);
    }

    public static Document parse(final MediaType contentType, final Reader reader)  throws JsonLdError {
        
        if (reader == null) {
            throw new IllegalArgumentException("The provided content reader is null.");
        }

        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }
        
        if (JsonDocument.accepts(contentType)) {
            return JsonDocument.of(contentType, reader);            
        }
        
        if (RdfDocument.accepts(contentType)) {
            return RdfDocument.of(contentType, reader);
        }
        
        return fireUnsupportedMediaType(contentType);
    }
    
    static final Document fireUnsupportedMediaType(MediaType contentType) throws JsonLdError {
        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, 
                "Unsupported media type '" + contentType 
                + "'. Supported content types are [" 
                + MediaType.JSON_LD + ", " 
                + MediaType.JSON  + ", +json"
                + MediaType.N_QUADS
                + "]"
                );
    }
    
}
