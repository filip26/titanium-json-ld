/*
 * Copyright 2020-2026 the original author or authors.
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
package com.apicatalog.jsonld.runtime;

import java.time.Duration;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.runtime.Execution.EventConsumer;

public final class TimeLimiter implements EventConsumer {

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
    public void term(EventType type, String key, Object value) throws JsonLdException {

        final var now = System.currentTimeMillis();

        final var elapsed = ttl - (now - start);

        if (elapsed <= 0) {
            throw new JsonLdException(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED);
        }
    }
}
