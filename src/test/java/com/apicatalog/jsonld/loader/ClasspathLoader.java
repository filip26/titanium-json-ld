package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;

public class ClasspathLoader implements DocumentLoader {

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        try (final InputStream is = getClass().getResourceAsStream(url.getPath())) {
            
            return JsonDocument.of(is);
            
        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }

}
