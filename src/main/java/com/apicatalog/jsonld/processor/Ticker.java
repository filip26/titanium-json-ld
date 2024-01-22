package com.apicatalog.jsonld.processor;

import java.time.Duration;
import java.time.Instant;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdOptions;

class Ticker extends ProcessingRuntime {

    Instant ticker;
    Duration ttl;

    Ticker(JsonLdOptions options) {
        super(options);
        this.ttl = options.getTimeout();
        this.ticker = Instant.now();
    }

    @Override
    public void tick() throws JsonLdError {

        final Instant now = Instant.now();

        ttl = ttl.minus(Duration.between(now, ticker).abs());

        if (ttl.isNegative()) {
            throw new JsonLdError(JsonLdErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }
        ticker = now;
    }

    @Override
    public void resetTicker() {
        ticker = Instant.now();
    }
}
