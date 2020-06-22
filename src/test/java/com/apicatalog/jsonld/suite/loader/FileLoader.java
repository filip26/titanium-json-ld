package com.apicatalog.jsonld.suite.loader;

import java.io.FileReader;
import java.io.IOException;
import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class FileLoader implements LoadDocumentCallback {

    @Override
    public RemoteDocument loadDocument(final URI uri, final LoadDocumentOptions options) throws JsonLdError {

        if (!"file".equalsIgnoreCase(uri.getScheme())) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        try {
            
            Document document = JsonDocument.parse(new FileReader(uri.toURL().getFile()));

            RemoteDocument remoteDocument = new RemoteDocument();
            remoteDocument.setDocument(document);
            remoteDocument.setDocumentUrl(uri);

            return remoteDocument;

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

}
