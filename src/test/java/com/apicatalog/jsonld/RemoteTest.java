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

import java.io.IOException;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.http.SimpleMockServer;
import com.apicatalog.jsonld.loader.UriBaseRewriter;
import com.apicatalog.jsonld.test.JsonLdMockServer;
import com.apicatalog.jsonld.test.JsonLdTestCase;
import com.apicatalog.jsonld.test.JsonLdTestManifest;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;

class RemoteTest {

    static SimpleMockServer mockServer;

    @BeforeAll
    static void proxyToWireMock() throws IOException {
        mockServer = new SimpleMockServer(8080);
        mockServer.start();
    }

    @AfterAll
    static void noMoreWireMock() {
        mockServer.stop();
        mockServer = null;
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testRemote(final JsonLdTestCase testCase) {

        // skip, HTML extraction is not supported yet
        assumeFalse("#t0013".equals(testCase.id));

        try {
            final var server = new JsonLdMockServer(
                    testCase,
                    JsonLdTestCase.TESTS_BASE,
                    JsonLdTestManifest.JSON_LD_API_BASE,
                    JsonLdTestSuite.ZIP_RESOURCE_LOADER);
System.out.println("XXXXXXXX");
            server.start(mockServer);

            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                JsonLdOptions expandOptions = JsonLdOptions.copyOf(options);

                expandOptions.loader(
                        new UriBaseRewriter(
                                JsonLdTestCase.TESTS_BASE,
                                mockServer.baseUrl(),
                                JsonLdTestSuite.HTTP_LOADER));

                return JsonLd.expand(testCase.input, expandOptions);
            });

            server.stop();

        } catch (JsonLdException e) {
            fail(e.getMessage());

        }
    }

    static final Stream<JsonLdTestCase> data() throws JsonLdException {
        return JsonLdTestManifest
                .load(
                        JsonLdTestManifest.JSON_LD_API_BASE,
                        "remote-doc-manifest.jsonld",
                        JsonLdTestSuite.ZIP_RESOURCE_LOADER)
                .stream()
                .filter(JsonLdTestCase.IS_NOT_V1_0) // skip specVersion == 1.0
        ;
    }
}
