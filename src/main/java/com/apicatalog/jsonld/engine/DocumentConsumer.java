package com.apicatalog.jsonld.engine;

import com.apicatalog.jsonld.document.Document;

@FunctionalInterface
public interface DocumentConsumer {

    void accept(Runtime runtime, String uri, Document document);
    
}
