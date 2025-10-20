package com.apicatalog.jsonld.document.cache;

public interface Cache<K, V> {

    boolean containsKey(final K key);

    V get(final K key);

    void put(final K key, V value);
}
