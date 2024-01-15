package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.cache.LruCache;
import com.apicatalog.jsonld.document.Document;

import java.net.URI;
import java.util.Collection;

public class LRUDocumentCache implements DocumentLoader {

    private final DocumentLoader documentLoader;

    private final LruCache<String, Document> cache;

    public LRUDocumentCache(int cacheSize) {
        this.documentLoader = SchemeRouter.defaultInstance();
        this.cache = new LruCache<>(cacheSize);
    }

    public LRUDocumentCache(DocumentLoader documentLoader, int cacheSize) {
        this.documentLoader = documentLoader;
        this.cache = new LruCache<>(cacheSize);
    }

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {
        String key = computeCacheKey(url, options);
        Document result = cache.get(key);
        if (result == null) {
            result = documentLoader.loadDocument(url, options);
            cache.put(key, result);
        }
        return result;
    }

    protected String computeCacheKey(URI url, DocumentLoaderOptions options) {
        // We can not use options.hashCode() as it does not return same
        // value for objects with same internal values.
        String optionsHash = options.getProfile() + ":" +
                options.isExtractAllScripts() + ":";
        Collection<String> profiles = options.getRequestProfile();
        if (profiles == null) {
            optionsHash += "null";
        } else {
            optionsHash += String.join(",", options.getRequestProfile());
        }
        //
        return url.toString() + ";" + optionsHash;
    }

}
