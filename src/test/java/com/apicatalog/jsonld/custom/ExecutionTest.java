package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;

class ExecutionTest {

    @Test
    void testExpandTimeout() {
        var ex = assertThrows(JsonLdException.class, () -> JsonLd.expand(
                Map.of(),
                Options.newOptions().timeout(Duration.ofNanos(0))));
        assertEquals(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED, ex.code());
    }

    @Test
    void testOnContextKey() throws JsonLdException {

        var document = Map.of(
                "@context", Map.of("name", "http://schema.org/name"),
                "name", "Alice");

        var options = Options.newOptions()
                .base("https://example.com/")
                .ordered(true);

        var keys = new ArrayList<>();

        var runtime = Execution.of(options);
        runtime.contextKeyCollector(keys::add);

        var expanded = Expander.expand(new TreeIO(document, NativeAdapter.instance()), options, runtime);
        
        assertNotNull(expanded);
        
        System.out.println(keys);

    }

}
