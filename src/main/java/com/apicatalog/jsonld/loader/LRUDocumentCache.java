package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.Objects;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.cache.LruCache;

public class LRUDocumentCache implements JsonLdLoader {

    private final JsonLdLoader documentLoader;

    private final LruCache<Object, Document> cache;

    protected static class CacheKey {

        private final URI url;

        private final LoaderOptions options;

        public CacheKey(URI url, LoaderOptions options) {
            this.url = url;
            this.options = options;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (other == null || getClass() != other.getClass()) {
                return false;
            }
            CacheKey cacheKey = (CacheKey) other;
            return Objects.equals(url, cacheKey.url) &&
                    Objects.equals(options, cacheKey.options);
        }

        @Override
        public int hashCode() {
            return Objects.hash(url, options);
        }
    }

    public LRUDocumentCache(JsonLdLoader documentLoader, int cacheSize) {
        this.documentLoader = documentLoader;
        this.cache = new LruCache<>(cacheSize);
    }

    @Override
    public Document loadDocument(URI url, LoaderOptions options) throws JsonLdError {
        Object key = createCacheKey(url, options);
        Document result = cache.get(key);
        if (result == null) {
            result = documentLoader.loadDocument(url, options);
            cache.put(key, result);
        }
        return result;
    }

    protected Object createCacheKey(URI url, LoaderOptions options){
        return new CacheKey(url, options);
    }

}
