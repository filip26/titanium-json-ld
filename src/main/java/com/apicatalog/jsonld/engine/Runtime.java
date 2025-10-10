package com.apicatalog.jsonld.engine;

public interface Runtime {

    void push(Action action);

    void fetch(String uri, DocumentConsumer consumer);

    <O> void complete(O output);

    <I> I input();
}
