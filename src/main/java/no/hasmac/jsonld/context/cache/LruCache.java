package no.hasmac.jsonld.context.cache;

import java.util.LinkedHashMap;
import java.util.Map;

public final class LruCache<K, V> implements Cache<K, V> {

    private final Map<K, V> cache;

    public LruCache(final int maxCapacity) {
        this.cache = new LinkedHashMap<K, V>((int)(maxCapacity / 0.75 + 1), 0.75f, true) {

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
