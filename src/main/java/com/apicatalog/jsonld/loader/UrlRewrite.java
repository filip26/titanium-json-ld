package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.RemoteDocument;

public final class UrlRewrite implements LoadDocumentCallback {

    private final String sourceBase;
    private final String targetBase;
    
    private final LoadDocumentCallback loader;
    
    public UrlRewrite(final String sourceBase, final String targetBase) {
        this(sourceBase, targetBase, new UrlConnectionLoader());
    }
    
    public UrlRewrite(final String sourceBase, final String targetBase, final LoadDocumentCallback loader) {
        this.sourceBase = sourceBase;
        this.targetBase = targetBase;
        
        this.loader = loader;
    }
    
    @Override
    public RemoteDocument loadDocument(final URI url, final LoadDocumentOptions options) throws JsonLdError {

        final String sourceUrl = url.toString();
        
        if (!sourceUrl.startsWith(sourceBase)) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        final String relativePath = sourceUrl.substring(sourceBase.length());

        final RemoteDocument remoteDocument = loader.loadDocument(URI.create(targetBase + relativePath), options);
        
        if (remoteDocument == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        remoteDocument.setDocumentUrl(url);
        return remoteDocument;

    }
}