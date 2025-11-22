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

import static org.junit.jupiter.api.Assertions.fail;

import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.ClasspathLoader;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.StaticLoader;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.jsonld.runtime.TermMapCollector;
import com.apicatalog.jsonld.test.JunitRunner;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.web.media.MediaType;

class PoliciesTest {

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
    @MethodSource("data")
    void testPolicy(TestCase testCase) {

        final var termMap = new LinkedHashMap<String, Object>();

        var termMapper = new TermMapCollector(termMap::put);
        try {
            var options = testCase.getOptions();

            options.loader(StaticLoader.newBuilder()
                    .parser(MediaType.JSON_LD, JakartaTestSuite.PARSER)
                    .classpath("https://www.w3.org/ns/credentials/v2", "/com/apicatalog/jsonld/loader/credentials-v2.jsonld")
                    .classpath("https://w3id.org/vc-barcodes/v1", "/com/apicatalog/jsonld/loader/vc-barcodes-v1.jsonld")
                    .classpath("https://w3id.org/utopia/v2", "/com/apicatalog/jsonld/loader/utopia-v2-context.jsonld")
                    .fallback(options.loader())
                    .build());

            Expander.expand(
                    Document.load(testCase.input, options.loader()),
                    options,
                    Execution.of(options, termMapper));

            if (testCase.expectErrorCode != null) {
                JunitRunner.write(testCase, null, null, null);
                fail("Expected error " + testCase.expectErrorCode);                
            }

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
                        "policy-manifest.jsonld",
                        LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0); // skip specVersion == 1.0
    }
}
