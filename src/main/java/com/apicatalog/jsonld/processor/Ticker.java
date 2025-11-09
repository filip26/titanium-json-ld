/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.processor;

import java.time.Duration;
import java.time.Instant;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;

class Ticker {

    final Duration ttl;

    Instant ticker;

    Ticker(Duration ttl) {
        this.ttl = ttl;
        this.ticker = null;
    }

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
