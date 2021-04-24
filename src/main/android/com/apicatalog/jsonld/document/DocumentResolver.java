package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;

public class DocumentResolver {

    private static final Logger LOGGER = Logger.getLogger(DocumentResolver.class.getName());

    private MediaType fallbackContentType;
    
    public DocumentResolver() {
        this.fallbackContentType = null;
    }
    
    /**
     * Return a reader or throw {@link JsonLdError} if there is no reader nor fallbackContentType.
     * 
     * @param contentType content type of the requested reader
     * @return a reader allowing to transform an input into {@link Document}
     * @throws JsonLdError
     */
    public DocumentReader<InputStream> getReader(MediaType contentType) throws JsonLdError {
        
        DocumentReader<InputStream> reader = findReader(contentType);
        
        if (reader != null) {
            return reader;
        }
        
        if (fallbackContentType != null) {
            LOGGER.log(Level.WARNING, "Content type [{0}] is not supported, trying again with [{1}].", new Object[] { contentType, fallbackContentType});
            reader = findReader(fallbackContentType);
            
            if (reader != null) {
                return reader;
            }
        }      
        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                    "Unsupported media type '" + contentType
                    + "'. Supported content types are ["
                    + MediaType.JSON_LD + ", "
                    + MediaType.JSON  + ", +json, "
                    + (Rdf.canRead().stream().map(MediaType::toString).collect(Collectors.joining(", ")))
                    + "]"
                    );
    }

    public void setFallbackContentType(MediaType fallbackContentType) {
        this.fallbackContentType = fallbackContentType;
    }
    
    private static final DocumentReader<InputStream> findReader(final MediaType type) {
        
        if (JsonDocument.accepts(type)) {
            return is ->  JsonDocument.of(type, is);
        }

        if (RdfDocument.accepts(type)) {
            return is -> RdfDocument.of(type, is);
        }        
        
        return null;
    }
}
