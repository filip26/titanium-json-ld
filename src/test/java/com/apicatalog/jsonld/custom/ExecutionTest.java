package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Duration;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;

class ExecutionTest {

    @Test
    void testExpandTimeout() {
        var ex = assertThrows(JsonLdException.class, () -> JsonLd.expand(
                Map.of(),
                Options.newOptions().timeout(Duration.ofNanos(0))));
        assertEquals(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED, ex.code());
    }

}
