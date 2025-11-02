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

class HttpLoaderTest {

//    WireMockServer wireMockServer;
//
//    @BeforeEach
//    void proxyToWireMock() {
//        wireMockServer = new WireMockServer(WireMockConfiguration.options());
//        wireMockServer.start();
//    }
//
//    @AfterEach
//    void noMoreWireMock() {
//        wireMockServer.stop();
//        wireMockServer = null;
//    }
//
//    @Test
//    void testMissingContentType() throws URISyntaxException, JsonLdException {
//
//        final JsonLdTestCase testCase = JsonLdTestManifest
//                .load("/com/apicatalog/jsonld/test/", "manifest.json", JsonLdTestSuite.CLASSPATH_LOADER)
//                .stream()
//                .filter(o -> "#t0002".equals(o.id))
//                .findFirst().orElseThrow(() -> new NoSuchElementException());
//
//        testCase.options.contentType = null;
//
//        execute(testCase);
//    }
//
//    @Test
//    void testPlainTextContentType() throws URISyntaxException, JsonLdException {
//
//        final JsonLdTestCase testCase = JsonLdTestManifest
//                .load("/com/apicatalog/jsonld/test/", "manifest.json", JsonLdTestSuite.CLASSPATH_LOADER)
//                .stream()
//                .filter(o -> "#t0008".equals(o.id))
//                .findFirst().orElseThrow(() -> new NoSuchElementException());
//
//        execute(testCase);
//    }
//
//    void execute(JsonLdTestCase testCase) {
//        JsonLdMockServer server = new JsonLdMockServer(testCase, testCase.baseUri.substring(0, testCase.baseUri.length() - 1), "/com/apicatalog/jsonld/test/", JsonLdTestSuite.CLASSPATH_LOADER);
//
//        try {
//
//            server.start();
//
//            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {
//
//                JsonLdOptions expandOptions = JsonLdOptions.copyOf(options);
//
//                expandOptions.loader(
//                                    new UriBaseRewriter(
//                                                testCase.baseUri,
//                                                wireMockServer.baseUrl() + "/",
//                                                JsonLdTestSuite.HTTP_LOADER));
//
//                return null;
//                //FIXME
////                return JsonDocument.of(JsonLd.expand(testCase.input).options(expandOptions).get());
//            });
//
//            server.stop();
//
//        } catch (Exception e) {
//            fail(e.getMessage());
//        }
//    }

}
