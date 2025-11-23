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
import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.jsonld.runtime.TypeMapCollector;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.java.NativeAdapter;
import com.apicatalog.web.media.MediaType;

class TypeMapTest {

    static ClasspathLoader LOADER = ClasspathLoader
            .newBuilder()
            .defaultParser(JakartaTestSuite.PARSER)
            .build();

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testTypeMapper(TestCase testCase) throws JsonLdException, TreeIOException, IOException {
        final var typeMap = new LinkedHashMap<String, Object>();

        final var typeMapper = new TypeMapCollector(typeMap::put);

        try {
            var options = testCase.getOptions();
            
            options.loader( StaticLoader.newBuilder()
                    .parser(MediaType.JSON_LD, JakartaTestSuite.PARSER)
                    .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
                    .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
                    .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
                    .fallback(options.loader())
                    .build());
                    
            var runtime = Execution.of(options, typeMapper);

            var expanded = Expander.expand(
                    Document.load(testCase.input, options.loader()),
                    options,
                    runtime);

            assertNotNull(expanded);

            TermMapTest.validateJson(
                    testCase,
                    options,
                    typeMap,
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
