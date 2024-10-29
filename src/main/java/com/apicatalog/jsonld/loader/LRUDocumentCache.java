package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import com.apicatalog.jsonld.context.cache.LruCache;
import com.apicatalog.jsonld.document.Document;

public class LRUDocumentCache implements DocumentLoader {

    private final DocumentLoader documentLoader;

    private final LruCache<Object, Document> cache;

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

    public LRUDocumentCache(DocumentLoader documentLoader, int cacheSize) {
        this.documentLoader = documentLoader;
        this.cache = new LruCache<>(cacheSize);
    }

    @Override
    public CompletableFuture<Document> loadDocument(URI url, DocumentLoaderOptions options) {
        Object key = createCacheKey(url, options);
        Document result = cache.get(key);
        if (result == null) {
            return documentLoader.loadDocument(url, options)
                    .thenApply(document -> {
                        cache.put(key, document);
                        return document;
                    });
        }
        return CompletableFuture.completedFuture(result);
    }

    protected Object createCacheKey(URI url, DocumentLoaderOptions options) {
        return new CacheKey(url, options);
    }

}
