package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class MockLoader implements LoadDocumentCallback {

    private final JsonStructure response;
    
    public MockLoader(final JsonStructure response) {
        this.response = response;
    }

    @Override
    public RemoteDocument loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError {

        final RemoteDocument remoteDocument = RemoteDocument.of(response);
        remoteDocument.setDocumentUrl(url);
        
        return remoteDocument;
    }
    
    
    
}
