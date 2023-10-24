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
package no.hasmac.jsonld.loader;

import static org.junit.jupiter.api.Assertions.fail;

import java.net.URISyntaxException;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.test.JsonLdManifestLoader;
import no.hasmac.jsonld.test.JsonLdMockServer;
import no.hasmac.jsonld.test.JsonLdTestCase;
import no.hasmac.jsonld.test.JsonLdTestRunnerJunit;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;

class HttpLoaderTest {

    WireMockServer wireMockServer;

    @BeforeEach
    void proxyToWireMock() {
        wireMockServer = new WireMockServer(WireMockConfiguration.options());
        wireMockServer.start();
    }

    @AfterEach
    void noMoreWireMock() {
        wireMockServer.stop();
        wireMockServer = null;
    }

    @Test
    void testMissingContentType() throws URISyntaxException, JsonLdError {

        final JsonLdTestCase testCase = JsonLdManifestLoader
                .load("/no/hasmac/jsonld/test/", "manifest.json", new ClasspathLoader())
                .stream()
                .filter(o -> "#t0002".equals(o.id))
                .findFirst().orElseThrow(() -> new NoSuchElementException());

        testCase.contentType = null;

        execute(testCase);
    }

    @Test
    void testPlainTextContentType() throws URISyntaxException, JsonLdError {

        final JsonLdTestCase testCase = JsonLdManifestLoader
                .load("/no/hasmac/jsonld/test/", "manifest.json", new ClasspathLoader())
                .stream()
                .filter(o -> "#t0008".equals(o.id))
                .findFirst().orElseThrow(() -> new NoSuchElementException());

        execute(testCase);
    }

    void execute(JsonLdTestCase testCase) {
        JsonLdMockServer server = new JsonLdMockServer(testCase, testCase.baseUri.substring(0, testCase.baseUri.length() - 1), "/no/hasmac/jsonld/test/", new ClasspathLoader());

        try {

            server.start();

            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                JsonLdOptions expandOptions = new JsonLdOptions(options);

                expandOptions.setDocumentLoader(
                                    new UriBaseRewriter(
                                                testCase.baseUri,
                                                wireMockServer.baseUrl() + "/",
                                                SchemeRouter.defaultInstance()));

                return JsonDocument.of(JsonLd.expand(testCase.input).options(expandOptions).get());
            });

            server.stop();

        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

}
