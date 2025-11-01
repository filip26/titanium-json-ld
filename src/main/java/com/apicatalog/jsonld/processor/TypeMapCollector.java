package com.apicatalog.jsonld.processor;

public interface TypeMapCollector {

    void beginMap(String key);

    void typeKeyName(String type);

    void end();

    boolean isTypeKey(String term);
}
