package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URLConnection;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;

public class UrlConnectionLoader implements LoadDocumentCallback {

    @Override
    public RemoteDocument loadDocument(final URI url, final LoadDocumentOptions options) throws JsonLdError {

        try {
            URLConnection connection = url.toURL().openConnection();

            //TODO set accept header
            //TODO set timeout
            //TODO set follow redirects
            
            connection.setDoInput(false);
            connection.setDoInput(true);
            
            connection.connect();
            
            //TODO check response headers
            
            Document document = JsonDocument.parse(new InputStreamReader(connection.getInputStream()));
            RemoteDocument remoteDocument = new RemoteDocument();
            remoteDocument.setDocument(document);
            remoteDocument.setDocumentUrl(url);    //TODO set final URL 

            return remoteDocument;
            
        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);            
        }        
    }

}
