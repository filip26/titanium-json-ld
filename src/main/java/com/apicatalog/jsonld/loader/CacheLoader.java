package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.cache.Cache;
import com.apicatalog.jsonld.cache.LruCache;
import com.apicatalog.jsonld.document.Document;

/**
 * A {@link DocumentLoader} that caches previously loaded JSON-LD documents.
 *
 * <p>
 * Used to prevent redundant retrieval or parsing of documents that are
 * repeatedly referenced during JSON-LD processing. Cached entries are keyed by
 * the document URI and loader {@link Options}.
 * </p>
 *
 * <p>
 * By default, caching is backed by an in-memory {@link LruCache}, but any
 * {@link Cache} implementation may be supplied.
 * </p>
 */
public final class CacheLoader implements DocumentLoader {

    private final DocumentLoader loader;
    private final Cache<Key, Document> cache;

    private static record Key(URI url, Options options) {
    }

    /**
     * Creates a new caching loader backed by a default {@link LruCache}.
     *
     * @param loader    the underlying {@link DocumentLoader} used for cache misses
     * @param cacheSize the maximum number of documents to retain in the cache
     */
    public CacheLoader(DocumentLoader loader, int cacheSize) {
        this(loader, new LruCache<>(cacheSize));
    }

    /**
     * Creates a new caching loader with a custom cache implementation.
     *
     * @param loader the underlying {@link DocumentLoader} used for cache misses
     * @param cache  the cache used to store loaded documents
     */
    public CacheLoader(DocumentLoader loader, Cache<Key, Document> cache) {
        this.loader = loader;
        this.cache = cache;
    }

    /**
     * Loads a JSON-LD document from cache if available, otherwise delegates to the
     * underlying loader and stores the result.
     *
     * @param url     the URI of the document to load
     * @param options loader configuration options
     * @return the loaded or cached {@link Document}
     * @throws JsonLdException if the underlying loader fails to retrieve the
     *                         document
     */
    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {
        var key = new Key(url, options);
        var result = cache.get(key);

        if (result == null) {
            result = loader.loadDocument(url, options);
            cache.put(key, result);
        }
        return result;
    }
}
