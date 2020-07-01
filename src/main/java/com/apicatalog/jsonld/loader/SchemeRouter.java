package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;

public final class SchemeRouter implements DocumentLoader {

    private final Map<String, DocumentLoader> loaders;
    
    public SchemeRouter() {
        this(new HttpLoader());
    }

    public SchemeRouter(HttpLoader httpLoader) {
        this.loaders = new LinkedHashMap<>();
        
        loaders.put("http", httpLoader);
        loaders.put("https", httpLoader);
        loaders.put("file", new FileLoader());
    }

    public SchemeRouter set(final String scheme, final DocumentLoader loader) {
        loaders.put(scheme, loader);
        return this;
    }

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        if (url == null) {
            throw new IllegalArgumentException("The url must not be null.");
        }
        
        final DocumentLoader loader = loaders.getOrDefault(url.getScheme().toLowerCase(), null);
        
        if (loader == null) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "URL scheme [" + url.getScheme() + "] is not supported.");
        }
        
        return loader.loadDocument(url, options);
    }
    
}
