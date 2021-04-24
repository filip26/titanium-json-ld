package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.util.Optional;
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
        return findReader(contentType)
                .or(() -> {
                    
                    if (fallbackContentType != null) {
                        LOGGER.log(Level.WARNING, "Content type [{0}] is not supported, trying again with [{1}].", new Object[] { contentType, fallbackContentType});
                        return findReader(fallbackContentType);
                    }
                    
                    return Optional.empty();
                })
                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                    "Unsupported media type '" + contentType
                    + "'. Supported content types are ["
                    + MediaType.JSON_LD + ", "
                    + MediaType.JSON  + ", +json, "
                    + (Rdf.canRead().stream().map(MediaType::toString).collect(Collectors.joining(", ")))
                    + "]"
                    ));
    }

    public void setFallbackContentType(MediaType fallbackContentType) {
        this.fallbackContentType = fallbackContentType;
    }
    
    private static final Optional<DocumentReader<InputStream>> findReader(final MediaType type) {
        
        if (type == null) {
            return Optional.empty();
        }
        
        if (JsonDocument.accepts(type)) {
            return Optional.of(is ->  JsonDocument.of(type, is));
        }

        if (RdfDocument.accepts(type)) {
            return Optional.of(is -> RdfDocument.of(type, is));
        }        
        
        return Optional.empty();
    }
}
