package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class MockLoader implements LoadDocumentCallback {

    private final JsonStructure response;
    
    public MockLoader(final JsonStructure response) {
        this.response = response;
    }

    @Override
    public Document loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError {

        final Document remoteDocument = JsonDocument.of(response);
        remoteDocument.setDocumentUrl(url);
        
        return remoteDocument;
    }
    
    
    
}
