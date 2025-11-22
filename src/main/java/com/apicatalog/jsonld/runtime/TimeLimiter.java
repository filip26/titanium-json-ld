package com.apicatalog.jsonld.runtime;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.EventProcessor;

public class TimeLimiter implements EventProcessor {
    
    private final long ttl;

    private long ticker;

    public TimeLimiter(Duration ttl) {
        this.ttl = ttl.toMillis();
        this.ticker = 0;
    }

    @Override
    public void onEvent(EventType type, String key, String value) throws JsonLdException {
        if (ticker == 0) {
            ticker = System.currentTimeMillis();
            return;
        }

        final var now = System.currentTimeMillis();

        final var elapsed = ttl - (now - ticker);

        if (elapsed <= 0) {
            ticker = 0;
            throw new JsonLdException(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }        
    }
}
