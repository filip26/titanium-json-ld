package com.apicatalog.jsonld.engine;

import java.util.function.Consumer;

public interface Runtime {

    void execute(Consumer<Runtime> fnc);

    void fetch(String uri, DocumentConsumer consumer);

    <O> void complete(O output);

    <I> I input();
}
