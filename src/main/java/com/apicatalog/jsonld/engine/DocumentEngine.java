package com.apicatalog.jsonld.engine;

import java.util.Set;

import com.apicatalog.jsonld.document.Document;

public interface DocumentEngine<R> {

    State state();
    
    void accept(String uri, Document document);
    
    Set<String> requests();
    
    R output();
    
}
