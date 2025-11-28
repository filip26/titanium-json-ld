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
package com.apicatalog.jsonld.loader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.test.MockServer;
import com.apicatalog.jsonld.test.TestManifest;

class HttpLoaderTest {

    static MockServer server = null;

    @BeforeAll
    static void startMockServer() throws JsonLdException {
        server = new MockServer(
                TestManifest.TESTS_BASE,
                TestManifest.JSON_LD_API_BASE);
        server.start();
    }

    @AfterAll
    static void stopMockServer() throws JsonLdException {
        if (server != null) {
            server.close();
            server = null;
        }
    }

    @Test
    void testMissingContentType() throws URISyntaxException, JsonLdException, IOException {

        server.when("/no-ct", 200, List.of(), readBytes("/com/apicatalog/jsonld/loader/document.json"));

        var remote = JakartaTestSuite.HTTP_LOADER.loadDocument(
                URI.create(server.baseUrl() + "/no-ct"),
                DocumentLoader.defaultOptions());

        assertNotNull(remote);
    }

    @Test
    void testPlainTextContentType() throws URISyntaxException, IOException {

        server.when(
                "/text.plain",
                200,
                List.of(Map.entry("Content-Type", "text/plain")),
                readBytes("/com/apicatalog/jsonld/loader/document.json"));

        var ex = assertThrowsExactly(JsonLdException.class,
                () -> JakartaTestSuite.HTTP_LOADER
                        .loadDocument(
                                URI.create(server.baseUrl() + "/text.plain"),
                                DocumentLoader.defaultOptions()));

        assertEquals(ErrorCode.LOADING_DOCUMENT_FAILED, ex.code());
    }

    @Test
    void testAcceptAnyContentType() throws URISyntaxException, IOException, JsonLdException {

        server.when(
                "/text.plain",
                200,
                List.of(Map.entry("Content-Type", "text/plain")),
                readBytes("/com/apicatalog/jsonld/loader/document.json"));

        var remote = JakartaTestSuite.HTTP_LOADER
                .accept(contentType -> true)
                .loadDocument(
                        URI.create(server.baseUrl() + "/text.plain"),
                        DocumentLoader.defaultOptions());

        // reset shared instance state
        JakartaTestSuite.HTTP_LOADER.accept(HttpLoader.JSON_LD_CONTENT);

        assertNotNull(remote);
    }

    @Test
    void testTimeout() {

        server.listen(Duration.ofSeconds(5));

        var ex = assertThrowsExactly(JsonLdException.class,
                () -> JakartaTestSuite.HTTP_LOADER
                        .timeout(Duration.ofMillis(200))
                        .loadDocument(
                                URI.create(server.baseUrl() + "/text.plain"),
                                DocumentLoader.defaultOptions()));

        server.hangup();

        // reset shared instance state
        JakartaTestSuite.HTTP_LOADER.timeout(Duration.ofSeconds(5));

        assertEquals(ErrorCode.LOADING_DOCUMENT_TIMEOUT, ex.code());
    }

    private final byte[] readBytes(final String name) throws IOException {
        try (final var is = getClass().getResourceAsStream(name)) {
            return is.readAllBytes();
        }
    }

}
