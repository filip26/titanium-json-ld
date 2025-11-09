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

import java.util.LinkedHashMap;
import java.util.Map;

public final class LruCache<K, V> implements CacheLoader.Cache<K, V> {

    private final Map<K, V> cache;

    public LruCache(final int maxCapacity) {
        this.cache = new LinkedHashMap<K, V>((int) (maxCapacity / 0.75 + 1), 0.75f, true) {

            private static final long serialVersionUID = 4822962879473741809L;

            @Override
            protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
                return this.size() > maxCapacity;
            }
        };
    }

    @Override
    public boolean containsKey(final K key) {
        return cache.containsKey(key);
    }

    @Override
    public V get(final K key) {
        return cache.get(key);
    }

    @Override
    public void put(final K key, V value) {
        cache.put(key, value);
    }

    public long size() {
        return cache.size();
    }
}
