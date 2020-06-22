package com.apicatalog.jsonld.suite.loader;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class ClassPathLoader implements LoadDocumentCallback {

    @Override
    public RemoteDocument loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError {
        
        if (!"classpath".equals(url.getScheme())) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        try (InputStream is = getClass().getResourceAsStream(url.toString().substring("classpath:".length()))) {

            if (is == null) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
            }
            
            Document document = JsonDocument.parse(new InputStreamReader(is));
            
            RemoteDocument remoteDocument = new RemoteDocument();
            remoteDocument.setDocument(document);
            remoteDocument.setDocumentUrl(url); 

            return remoteDocument;
            
            
        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

}
