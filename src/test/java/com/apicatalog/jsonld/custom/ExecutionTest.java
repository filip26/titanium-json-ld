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
import java.io.StringWriter;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.ExecutionEvents;
import com.apicatalog.jsonld.processor.ExecutionEvents.TermMapper;
import com.apicatalog.jsonld.processor.ExecutionEvents.TypeMapper;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.media.MediaType;

import jakarta.json.JsonWriter;

class ExecutionTest {

    static final DocumentLoader UTOPIA_LOADER = StaticLoader.newBuilder()
            .parser(MediaType.JSON_LD, JakartaTestSuite.PARSER)
            .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
            .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
            .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
            .build();

    @Test
    void testExpandTimeout() {
        var ex = assertThrows(JsonLdException.class, () -> JsonLd.expand(
                Map.of(),
                Options.newOptions().timeout(Duration.ofNanos(0))));
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
    void testContextKeyCollector(String input, String output) throws JsonLdException, TreeIOException, IOException {

        var document = read(input);

        var options = Options.with(UTOPIA_LOADER);

        var keys = new ArrayList<Collection<String>>();

        var runtime = ExecutionEvents.of(options);
        runtime.contextKeyCollector(keys::add);

        var expanded = Expander.expand(document, options, runtime);

        assertNotNull(expanded);

//        System.out.println(keys
//                .stream()
//                .map(c -> c.stream().filter(Predicate.not(Keywords::contains)).sorted())
//                .flatMap(Function.identity())
//                .collect(Collectors.toCollection(LinkedHashSet::new)));

        var expected = read(output);

        var match = TreeIO.deepEquals(Map.of("contextKeys", keys), NativeAdapter.instance(), expected.node(), expected.adapter());

        if (!match) {
            var out = new StringWriter();

            try (final JsonWriter jsonWriter = JunitRunner.JSON_WRITER_FACTORY.createWriter(out)) {
                jsonWriter.write(new JakartaMaterializer().node(keys, NativeAdapter.instance()));
            }
            System.out.println(out);
        }

        assertTrue(match);
    }

    static Stream<Arguments> keyTypeMapCases() {
        return Stream.of(
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-typemap.json"),
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia-2.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-typemap-2.json"));
    }

    @ParameterizedTest
    @MethodSource("keyTypeMapCases")
    void testTypeMapper(String input, String output) throws JsonLdException, TreeIOException, IOException {

        var document = read(input);

        var options = Options.with(UTOPIA_LOADER);

        var typeMap = new LinkedHashMap<String, Object>();

        var typeMapper = new TypeMapper() {

            Deque<Map<String, Object>> stack = new ArrayDeque<>();

            {
                stack.push(typeMap);
            }

            @Override
            public void onBeginMap(String key) {
                var map = new LinkedHashMap<String, Object>();
                stack.peek().put(key, map);
                stack.push(map);
            }

            @Override
            public void onEndMap(String key) {
                stack.pop();
            }

            @Override
            public void onType(String key, String id) {
                stack.peek().put(key, id);
            }
        };

        var runtime = ExecutionEvents.of(options);
        runtime.keyTypeMapper(typeMapper);

        var expanded = Expander.expand(document, options, runtime);
        assertNotNull(expanded);

        var expected = read(output);

        var match = TreeIO.deepEquals(typeMap, NativeAdapter.instance(), expected.node(), expected.adapter());

        if (!match) {
            var out = new StringWriter();

            out.write("Expected\n");
            try (final JsonWriter jsonWriter = JunitRunner.JSON_WRITER_FACTORY.createWriter(out)) {
                jsonWriter.write(new JakartaMaterializer().node(expected));
            }
            out.flush();
            out.write("\nActual\n");

            try (final JsonWriter jsonWriter = JunitRunner.JSON_WRITER_FACTORY.createWriter(out)) {
                jsonWriter.write(new JakartaMaterializer().node(typeMap, NativeAdapter.instance()));
            }
            
            System.out.println(out);
        }

        assertTrue(match);
    }

    static Stream<Arguments> termMapCases() {
        return Stream.of(
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-termmap.json"),
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia-2.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-termmap-2.json"));
    }

    @ParameterizedTest
    @MethodSource("termMapCases")
    void testTermMapper(String input, String output) throws JsonLdException, TreeIOException, IOException {

        var document = read(input);

        var options = Options.with(UTOPIA_LOADER);
        
        var termMap = new LinkedHashMap<String, Object>();

        var termMapper = new TermMapper() {

            Deque<Map<String, Object>> stack = new ArrayDeque<>();

            {
                stack.push(termMap);
            }

            @Override
            public void onBeginMap(String key) {
                var map = new LinkedHashMap<String, Object>();

                var origin = stack.peek().get(key);
                if (origin != null) {
                    map.put(Keywords.ID, origin);
                }
                
                stack.peek().put(key, map);
                stack.push(map);
            }

            @Override
            public void onEndMap(String key) {
                stack.pop();
            }

            @Override
            public void onTerm(String key, String uri) {
                stack.peek().put(key, uri);
            }
        };

        var runtime = ExecutionEvents.of(options);
        runtime.termMapper(termMapper);

        var expanded = Expander.expand(document, options, runtime);
        assertNotNull(expanded);
        
        System.out.println("XXXX" + termMap);

    }
    
    private final TreeIO read(final String name) throws JsonLdException, TreeIOException, IOException {
        try (final var is = getClass().getResourceAsStream(name)) {
            return JakartaTestSuite.PARSER.parse(is);
        }
    }
}
