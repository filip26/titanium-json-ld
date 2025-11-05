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
package com.apicatalog.jsonld;

import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.loader.UriBaseRewriter;
import com.apicatalog.jsonld.test.MockServer;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.jsonld.test.JunitRunner;

class RemoteTest {

    static MockServer server;

    @BeforeAll
    static void startMockServer() throws JsonLdException {
        server = new MockServer(
                TestManifest.TESTS_BASE,
                TestManifest.JSON_LD_API_BASE);
        server.start();
    }

    @AfterAll
    static void stopMockServer() throws JsonLdException {
        server.close();
        server = null;
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testRemote(final TestCase testCase) {

        // skip, HTML extraction is not supported yet
        assumeFalse("#t0013".equals(testCase.id));

        try {
            server.setup(testCase);

            (new JunitRunner(testCase)).execute(options -> {

                Options expandOptions = Options.copyOf(options);

                expandOptions.loader(
                        new UriBaseRewriter(
                                TestManifest.TESTS_BASE,
                                server.baseUrl(),
                                JsonLdTestSuite.HTTP_LOADER));

                return JsonLd.expand(testCase.input, expandOptions);
            });

        } catch (JsonLdException e) {
            fail(e.getMessage());
        }
    }

    static final Stream<TestCase> data() throws JsonLdException {
        return TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "remote-doc-manifest.jsonld",
                        JsonLdTestSuite.ZIP_RESOURCE_LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
        ;
    }
}
