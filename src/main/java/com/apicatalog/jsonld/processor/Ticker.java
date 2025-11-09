package com.apicatalog.jsonld.processor;

import java.time.Duration;
import java.time.Instant;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;

class Ticker extends Execution {

    final Duration ttl;

    Instant ticker;

    Ticker(Options options) {
        this.ttl = options.timeout();
        this.ticker = null;
    }

    @Override
    public void tick() throws JsonLdException {
        if (ticker == null) {
            ticker = Instant.now();
            return;
        }

        final Instant now = Instant.now();

        var elapsed = ttl.minus(Duration.between(now, ticker).abs());

        if (elapsed.isNegative()) {
            ticker = null;
            throw new JsonLdException(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }
    }
}
