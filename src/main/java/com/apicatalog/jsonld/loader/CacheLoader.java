/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.loader;

import java.net.URI;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;

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

    /**
     * Simple in-memory cache interface used by {@link CacheLoader} and related
     * components.
     *
     * @param <K> key type
     * @param <V> value type
     */
    public interface Cache<K, V> {

        /**
         * Returns {@code true} if this cache contains a value for the specified key.
         *
         * @param key the key to check
         * @return {@code true} if a value is cached, otherwise {@code false}
         */
        boolean containsKey(final K key);

        /**
         * Returns the cached value associated with the given key, or {@code null} if no
         * value is present.
         *
         * @param key the key to look up
         * @return the cached value, or {@code null}
         */
        V get(final K key);

        /**
         * Stores the given key–value pair in the cache.
         *
         * @param key   the key to store
         * @param value the value to cache
         */
        void put(final K key, V value);
    }
}
