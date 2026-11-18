/*
 * Copyright 2025 the original author or authors.
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
package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.jsonld.runtime.Execution.EventType;
import com.apicatalog.jsonld.suite.SuiteConfig;
import com.apicatalog.web.media.MediaType;

class ExecutionTest {

    static final DocumentLoader UTOPIA_LOADER = StaticLoader.newBuilder()
            .reader(MediaType.JSON_LD, SuiteConfig::read)
            .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
            .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
            .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
            .build();

    @Test
    void testExpandTimeout() throws JsonLdException, IOException {

        var document = read("/com/apicatalog/jsonld/test/vc-utopia.jsonld");

        var options = Options.with(UTOPIA_LOADER)
                .timeout(Duration.ofNanos(0));

        var ex = assertThrows(JsonLdException.class, () -> JsonLd.expand(
                (Map) document,
                options));

        assertEquals(ErrorCode.PROCESSING_TIMEOUT_EXCEEDED, ex.code());
    }

    static Stream<Arguments> contextKeysCases() {
        return Stream.of(
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-terms.json"),
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia-2.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-terms-2.json"));
    }

    @ParameterizedTest
    @MethodSource("contextKeysCases")
    void testContextKeyCollector(String input, String output) throws JsonLdException, IOException {

        var document = read(input);

        var options = Options.with(UTOPIA_LOADER);

        final var keys = new ArrayList<Collection<String>>();

        var runtime = Execution.of(options).add((type, values) -> {
            if (EventType.CONTEXT_KEYS == type) {
                keys.add(values);
            }
        });

        var expanded = Expander.expand((Map) document, options, runtime);

        assertNotNull(expanded);

//        System.out.println(keys
//                .stream()
//                .map(c -> c.stream().filter(Predicate.not(Keywords::contains)).sorted())
//                .flatMap(Function.identity())
//                .collect(Collectors.toCollection(LinkedHashSet::new)));

        var expected = read(output);

//FIXME        var match = expected.isIsomorphicTo(Map.of("contextKeys", keys), JavaAdapter.instance());

        var match = false;
        // TreeIO.deepEquals(Map.of("contextKeys", keys), NativeAdapter.instance(),
        // expected.node(), expected.adapter());

        if (!match) {
            var out = new StringWriter();

//            try (final JsonWriter jsonWriter = JunitRunner.JSON_GENERATOR_FACTORY.createWriter(out)) {
//       //FIXME         jsonWriter.write(JakartaMaterializer.node(keys, JavaAdapter.instance()));
//            }
            System.out.println(out);
        }

        assertTrue(match);
    }

    private final Object read(final String name) throws JsonLdException, IOException {
        try (final var reader = new InputStreamReader(getClass().getResourceAsStream(name), StandardCharsets.UTF_8)) {
            return SuiteConfig.read(MediaType.JSON, reader);
        }
    }
}
