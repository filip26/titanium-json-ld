package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.cache.LruCache;
import com.apicatalog.jsonld.document.Document;

import java.net.URI;
import java.util.Objects;

public class LRUDocumentCache implements DocumentLoader {

    protected static class CacheKey {

        private final URI url;

        private final DocumentLoaderOptions options;

        public CacheKey(URI url, DocumentLoaderOptions options) {
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

    private final DocumentLoader documentLoader;

    private final LruCache<Object, Document> cache;

    public LRUDocumentCache(DocumentLoader documentLoader, int cacheSize) {
        this.documentLoader = documentLoader;
        this.cache = new LruCache<>(cacheSize);
    }

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {
        Object key = createCacheKey(url, options);
        Document result = cache.get(key);
        if (result == null) {
            result = documentLoader.loadDocument(url, options);
            cache.put(key, result);
        }
        return result;
    }

    protected Object createCacheKey(URI url, DocumentLoaderOptions options){
        return new CacheKey(url, options);
    }

}
