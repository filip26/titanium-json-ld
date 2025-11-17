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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.SuiteEvironment;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.ExecutionEvents;
import com.apicatalog.jsonld.processor.ExecutionEvents.TermMapper;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.media.MediaType;

class TermMapTest {

    static ClasspathLoader LOADER = ClasspathLoader
            .newBuilder()
            .defaultParser(JakartaTestSuite.PARSER)
            .build();

    static final DocumentLoader UTOPIA_LOADER = StaticLoader.newBuilder()
            .parser(MediaType.JSON_LD, JakartaTestSuite.PARSER)
            .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
            .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
            .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
            .build();

    static Stream<Arguments> termMapCases() {
        return Stream.of(
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-termmap.json"),
                Arguments.of(
                        "/com/apicatalog/jsonld/test/vc-utopia-2.jsonld",
                        "/com/apicatalog/jsonld/test/vc-utopia-termmap-2.json"));
    }

//    @ParameterizedTest
//    @MethodSource("termMapCases")
//    void testTermMapper(String input, String output) throws JsonLdException, TreeIOException, IOException {
    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testTermMapper(TestCase testCase) {

//        var document = read(testCase.input);
//
//        var options = Options.with(UTOPIA_LOADER);

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
                    if (origin instanceof Collection col) {
                        col.add(map);
                        stack.push(map);
                        return;
                    }
                    var col = new ArrayList<>();
                    col.add(origin);
                    col.add(map);
                    stack.peek().put(key, col);
                    stack.push(map);
                    return;
                }
                stack.peek().put(key, map);
                stack.push(map);
            }

            @Override
            public void onEndMap(String key) {
                if (stack.peek().isEmpty()) {
//
                    stack.pop();
                    stack.peek().remove(key);
//
////                } else if (stack.peek().size() == 1 && stack.peek().containsKey(Keywords.ID)) {
////
////                    final var term = stack.pop().get(Keywords.ID);
////
////                    stack.peek().put(key, term);
                } else {
                    stack.pop();
                }
            }

            @Override
            public void onTerm(String key, String uri) {
                if (stack.peek().get(key) instanceof Map || stack.peek().get(key) instanceof Collection) {
                    var map = (Map<String, String>)stack.peek().computeIfAbsent("@terms", v -> new HashMap<String, String>());
                    map.put(key, uri);
                } else {
                    stack.peek().put(key, uri);
                }
            }
        };
        try {
            var options = testCase.getOptions();// .loader(JakartaTestSuite.LOADER);

            var runtime = ExecutionEvents.of(options);
            runtime.termMapper(termMapper);

            var expanded = Expander.expand(
                    Document.load(testCase.input, options.loader()),
                    options,
                    runtime);

            assertNotNull(expanded);

            System.out.println("XXXX" + termMap);

            JunitRunner.validateJsonLd(
                    testCase,
                    options,
                    termMap,
                    NativeAdapter.instance());

        } catch (JsonLdException e) {

            if (Objects.equals(e.code(), testCase.expectErrorCode)) {
                return;
            }

            JunitRunner.write(testCase, null, null, e);

            if (testCase.expectErrorCode != null) {
                fail("Unexpected error " + e.code() + ", exptected " + testCase.expectErrorCode + ".");
            } else {
                fail("Unexpected error " + e + ".");
            }
        }

    }

    static final Stream<TestCase> data() throws JsonLdException {
        return TestManifest
                .load(
                        "classpath:/com/apicatalog/jsonld/",
                        "termmap-manifest.jsonld",
                        LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0); // skip specVersion == 1.0
    }

    private final TreeIO read(final String name) throws JsonLdException, TreeIOException, IOException {
        try (final var is = getClass().getResourceAsStream(name)) {
            return JakartaTestSuite.PARSER.parse(is);
        }
    }
}
