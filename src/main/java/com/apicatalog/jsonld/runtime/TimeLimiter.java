package com.apicatalog.jsonld.runtime;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.EventProcessor;

public final class TimeLimiter implements EventProcessor {

    private final long ttl;

    private final long start;

    private TimeLimiter(Duration ttl, long start) {
        this.ttl = ttl.toMillis();
        this.start = start;
    }

    public static TimeLimiter of(Duration ttl) {
        return new TimeLimiter(ttl, System.currentTimeMillis());
    }

    @Override
    public void onEvent(EventType type, String key, String value) throws JsonLdException {

        final var now = System.currentTimeMillis();

        final var elapsed = ttl - (now - start);

        if (elapsed <= 0) {
            throw new JsonLdException(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }
    }
}
