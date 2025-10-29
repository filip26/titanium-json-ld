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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.jsonld.loader.SchemeRouter;
import com.apicatalog.jsonld.loader.UriBaseRewriter;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.jsonld.test.JsonLdTestManifest;
import com.apicatalog.jsonld.test.JsonLdMockServer;
import com.apicatalog.jsonld.test.JsonLdTestCase;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;

class RemoteTest {

    WireMockServer wireMockServer;

    @BeforeEach
    public void proxyToWireMock() {
        wireMockServer = new WireMockServer(WireMockConfiguration.options());
        wireMockServer.start();
    }

    @AfterEach
    public void noMoreWireMock() {
        wireMockServer.stop();
        wireMockServer = null;
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testRemote(final JsonLdTestCase testCase) {

        // skip, HTML extraction is not supported yet
        assumeFalse("#t0013".equals(testCase.id));

        try {

            JsonLdMockServer server = new JsonLdMockServer(testCase, JsonLdTestCase.TESTS_BASE, JsonLdTestManifest.JSON_LD_API_BASE, new ZipResourceLoader());

            server.start();

            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                JsonLdOptions expandOptions = new JsonLdOptions(options);

                expandOptions.setDocumentLoader(
                                    new UriBaseRewriter(
                                                JsonLdTestCase.TESTS_BASE,
                                                wireMockServer.baseUrl(),
                                                SchemeRouter.defaultInstance()));

                return JsonLd.expand(testCase.input).options(expandOptions).get();
            });

            server.stop();

        } catch (JsonLdError e) {
            fail(e.getMessage());

        }
    }

    static final Stream<JsonLdTestCase> data() throws JsonLdError {
        return JsonLdTestManifest
                    .load(JsonLdTestManifest.JSON_LD_API_BASE, "remote-doc-manifest.jsonld", new ZipResourceLoader())
                    .stream()
                    .filter(JsonLdTestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                    ;
    }
}
