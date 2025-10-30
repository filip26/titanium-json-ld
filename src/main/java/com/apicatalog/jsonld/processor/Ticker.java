package com.apicatalog.jsonld.processor;

import java.time.Duration;
import java.time.Instant;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;

class Ticker extends ProcessingRuntime {

    Instant ticker;
    Duration ttl;

    Ticker(JsonLdOptions options) {
        super(options);
        resetTicker();
    }

    @Override
    public void tick() throws JsonLdException {

        final Instant now = Instant.now();

        ttl = ttl.minus(Duration.between(now, ticker).abs());

        if (ttl.isNegative()) {
            throw new JsonLdException(JsonLdErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }
        ticker = now;
    }

    @Override
    public void resetTicker() {
        ttl = options.getTimeout();
        ticker = Instant.now();
    }
}
