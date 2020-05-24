package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.RemoteDocument;

public class UrlRewriteLoader implements LoadDocumentCallback {

    final String sourceBase;
    final String targetBase;
    
    final LoadDocumentCallback loader;
    
    public UrlRewriteLoader(final String sourceBase, final String targetBase) {
        this(sourceBase, targetBase, new UrlConnectionLoader());
    }
    
    public UrlRewriteLoader(final String sourceBase, final String targetBase, final LoadDocumentCallback loader) {
        this.sourceBase = sourceBase;
        this.targetBase = targetBase;
        
        this.loader = loader;
    }
    
    @Override
    public RemoteDocument loadDocument(URI url, LoadDocumentOptions options) throws JsonLdError {

        String sourceUrl = url.toString();
        
        if (!sourceUrl.startsWith(sourceBase)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        String realivePath = sourceUrl.substring(sourceBase.length());

        RemoteDocument remoteDocument = loader.loadDocument(URI.create(targetBase + realivePath), options);
        
        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        remoteDocument.setDocumentUrl(url);
        return remoteDocument;

    }
}
