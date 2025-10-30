package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.cache.Cache;
import com.apicatalog.jsonld.cache.LruCache;
import com.apicatalog.jsonld.document.Document;

public class CacheLoader implements DocumentLoader {

    private final DocumentLoader loader;

    private final Cache<Key, Document> cache;

    protected static record Key(URI url, LoaderOptions options) {
    }

    public CacheLoader(DocumentLoader loader, int cacheSize) {
        this(loader, new LruCache<>(cacheSize));
    }

    public CacheLoader(DocumentLoader loader, Cache<Key, Document> cache) {
        this.loader = loader;
        this.cache = cache;
    }

    @Override
    public Document loadDocument(URI url, LoaderOptions options) throws JsonLdError {
        var key = createCacheKey(url, options);
        var result = cache.get(key);
        
        if (result == null) {
            result = loader.loadDocument(url, options);
            cache.put(key, result);
        }
        return result;
    }

    protected Key createCacheKey(URI url, LoaderOptions options){
        return new Key(url, options);
    }

}
