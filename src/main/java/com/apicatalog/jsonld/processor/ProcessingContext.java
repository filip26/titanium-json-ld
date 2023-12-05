package com.apicatalog.jsonld.processor;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdError;

/**
 * A runtime context used during a transformation processing.
 * 
 * @since 1.4.0
 */
public class ProcessingContext {

    protected Duration timout;

    /**
     * Called in multiple places during a processing to check processing timeout if
     * set. Does nothing if timeout is not set.
     * 
     * When hit for the first time a timestamp is set, otherwise a duration is
     * decreased by timestamps difference.
     * 
     * @throws JsonLdError if a processing has exceeded
     */
    public void ping() throws JsonLdError {

    }

    /**
     * Update remaining time and resets timestamp. Is used top stop counting
     * processing time when an external library is called, e.g. a HTTP call.
     */
    public void pause() {

    }

    public ProcessingContext timout(Duration timout) {
        this.timout = timout;
        return this;
    }

}
