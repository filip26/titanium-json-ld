package com.apicatalog.jsonld.engine;

import java.util.Set;

import com.apicatalog.jsonld.document.Document;

/**
 * document processing engine
 * @param <R>
 */
public interface Engine<R> {

    public enum State {
        IDLE,
        PROCESSING,
        PENDING,
        SUCCEEDED,
        FAILED,
    }
    
    State state();
    
    void accept(String uri, Document document);
    
    Set<String> requests();
    
    R output();
    
}
