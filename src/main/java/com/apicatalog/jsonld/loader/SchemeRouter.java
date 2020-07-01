package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;

public final class SchemeRouter implements DocumentLoader {

    private static final DocumentLoader INSTANCE = 
                                new SchemeRouter()
                                        .set("http", HttpLoader.defaultInstance())
                                        .set("https", HttpLoader.defaultInstance())
                                        .set("file", new FileLoader());
    
    private final Map<String, DocumentLoader> loaders;
    
    public SchemeRouter() {
        this.loaders = new LinkedHashMap<>();
    }

    public static final DocumentLoader defaultInstance() {
        return INSTANCE;
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
