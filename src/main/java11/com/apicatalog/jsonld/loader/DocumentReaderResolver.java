package com.apicatalog.jsonld.loader;

import java.io.InputStream;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

public interface DocumentReaderResolver {

    /**
     * Return a reader or throw {@link JsonLdError} if there is no reader nor fallbackContentType.
     *
     * @param contentType content type of the requested reader
     * @return a reader allowing to transform an input into {@link Document}
     * @throws JsonLdError
     */
    DocumentReader<InputStream> getReader(MediaType contentType) throws JsonLdError;
    
}
