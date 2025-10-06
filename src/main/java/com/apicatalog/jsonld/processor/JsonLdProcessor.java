package com.apicatalog.jsonld.processor;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;

public interface JsonLdProcessor<I, O> {

    enum State {
        INIT, // initial state
        PROCESSING, // is processing
        CONTEXT_REQUEST, // expecting a document, an input
        COMPLETED, // completed the processing
    }

    // executes the processing, might stop accidently with Processing state and must
    // be subsequently
    // called until WAITING or COMPLETED state is reached.
    // must not be called again when WAITING state, it won't change unless {@code #document()} method is called.
    State execute(Duration duration) throws JsonLdError;
    
    State state();

    // when state is waiting, returns remote context URI to fetch and set by context method
    String requestedContext();
    
    // sets input document
    JsonLdProcessor<I, O> input(I document);
    
    // returns output document if the processor is in COMPLETED state
    O output();

    // adds a context
    // might be called prior execution. Must be called after WAITING state and pass
    // requested resolved document, then execute() will continue
    JsonLdProcessor<I, O> context(String uri, Document context);

}
