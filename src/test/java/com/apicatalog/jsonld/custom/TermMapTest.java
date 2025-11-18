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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.ExecutionEvents;
import com.apicatalog.jsonld.processor.ExecutionEvents.TermMapper;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
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
            .fallback(LOADER)
            .build();

    @ParameterizedTest(name = "{0}")
    @MethodSource( "data" )
    void testTermMapper(TestCase testCase) {

        final var termMap = new LinkedHashMap<String, Object>();

        var termMapper = new TermMapper() {

            final Deque<Object> path = new ArrayDeque<>();

            @Override
            public void onBeginMap(String key) {
                path.push(escapeJsonPointerSegment(key));
            }

            @Override
            public void onEndMap(String key) {
                path.pop();
            }

            @Override
            public void onBeginList(String key) {
                if (key != null) {
                    path.push(key);
                }
            }

            @Override
            public void onEndList(String key) {
                if (key != null) {
                    path.pop();
                }
            }

            @Override
            public void onTerm(String key, String uri) {

                if (path.isEmpty()) {
                    termMap.put("/" + escapeJsonPointerSegment(key), uri);

                } else {
                    var pointer = new ArrayList<String>(path.size());
                    path.stream().map(Object::toString).forEach(pointer::add);
                    Collections.reverse(pointer);
                    pointer.add(escapeJsonPointerSegment(key));

                    termMap.put("/" + String.join("/", pointer), uri);

                    if (path.peek() instanceof Integer order) {
                        path.pop();
                        path.push(order + 1);
                    }
                }
            }
        };
        try {
            var options = testCase.getOptions();
            
            options.loader( StaticLoader.newBuilder()
                    .parser(MediaType.JSON_LD, JakartaTestSuite.PARSER)
                    .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
                    .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
                    .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
                    .fallback(options.loader())
                    .build());
                    
            var runtime = ExecutionEvents.of(options).termMapper(termMapper);

            var expanded = Expander.expand(
                    Document.load(testCase.input, options.loader()),
                    options,
                    runtime);

            assertNotNull(expanded);

            validateJson(
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

    static boolean validateJson(final TestCase testCase, final Options options, final Object result, final TreeAdapter resultAdapter) {

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            var expectedDocument = options.loader().loadDocument(testCase.expect, DocumentLoader.defaultOptions());

            assertNotNull(expectedDocument);

            // compare expected with the result
            return compareJson(testCase, result, resultAdapter, expectedDocument.content());

        } catch (JsonLdException | TreeIOException e) {
            fail(e.getMessage());
        }
        return false;
    }

    public static final boolean compareJson(final TestCase testCase, final Object result, final TreeAdapter resultAdapter, final TreeIO expected) throws TreeIOException {

        if (TreeIO.deepEquals(expected.node(), expected.adapter(), result, resultAdapter)) {
            return true;
        }

        JunitRunner.write(testCase,
                new JakartaMaterializer().node(result, resultAdapter),
                new JakartaMaterializer().node(expected),
                null);

        fail("Expected " + expected.node() + ", but was" + result);
        return false;
    }

    /** Escape a single reference token (RFC6901): ~ -> ~0 and / -> ~1 */
    static String escapeJsonPointerSegment(String s) {
        Objects.requireNonNull(s, "segment");
        int n = s.length();
        StringBuilder sb = new StringBuilder(n + 4); // small growth room
        for (int i = 0; i < n; ++i) {
            char c = s.charAt(i);
            if (c == '~')
                sb.append("~0");
            else if (c == '/')
                sb.append("~1");
            else
                sb.append(c);
        }
        return sb.toString();
    }
}
