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
package no.hasmac.jsonld.test;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URI;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.http.link.Link;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.jsonld.loader.HttpLoader;
import no.hasmac.jsonld.loader.TestLoader;
import no.hasmac.jsonld.uri.UriResolver;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;

public final class JsonLdMockServer {

    private final JsonLdTestCase testCase;
    private final String testBase;
    private final String resourceBase;
    private final TestLoader loader;

    public JsonLdMockServer(JsonLdTestCase testCase, String testBase, String resourceBase, TestLoader loader) {
        this.testCase = testCase;
        this.testBase = testBase;
        this.resourceBase = resourceBase;
        this.loader = loader;
    }

    public void start() throws JsonLdError {

        String inputPath;

        if (testCase.redirectTo != null) {
            inputPath = testCase.redirectTo.toString();

        } else {
            inputPath = testCase.input.toString();
        }


        if (testCase.redirectTo != null) {
            stubFor(get(urlEqualTo(testCase.input.toString().substring(testBase.length())))
                    .willReturn(aResponse()
                        .withStatus(testCase.httpStatus)
                        .withHeader("Location", testCase.redirectTo.toASCIIString().substring(testBase.length()))
                            ));
        }

        if (testCase.httpLink != null && testCase.httpLink.size() == 1) {

            String linkValue = testCase.httpLink.iterator().next();

            Link link = Link.of(linkValue, URI.create(".")).iterator().next();

            final MediaType contentType;

            if (link.type().isPresent()) {

                contentType = link.type().get();

            } else {

                if (link.target().toString().endsWith(".html")) {
                    contentType = MediaType.HTML;

                } else if (link.target().toString().endsWith(".jsonld")) {
                    contentType = MediaType.JSON_LD;

                } else if (link.target().toString().endsWith(".json")) {
                    contentType = MediaType.JSON;

                } else {
                    contentType = null;
                }
            }

            assertNotNull(contentType);

            String linkUri = UriResolver.resolve(testCase.input, link.target().toString());

            byte[] content  = loader.fetchBytes(URI.create(resourceBase +  linkUri.substring(testCase.baseUri.length())));

            if (content != null) {
//                    Assert.assertNotNull(linkedDocument);
//                    Assert.assertNotNull(linkedDocument.getContent());

//                    linkedDocument.getContent().getBytes().ifPresent(byteArray -> {

                stubFor(get(urlEqualTo(linkUri.substring(testBase.length())))
                        .willReturn(aResponse()
                            .withStatus(200)
                            .withHeader("Content-Type", contentType.toString())
                            .withBody(content))
                                );
//                    });
            }
        }

        ResponseDefinitionBuilder mockResponseBuilder = aResponse();

        byte[] content = loader.fetchBytes(URI.create(resourceBase + inputPath.substring(testCase.baseUri.length())));

        if (content != null) {
            mockResponseBuilder.withStatus(200);

            if (testCase.httpLink != null) {
                testCase.httpLink.forEach(link -> mockResponseBuilder.withHeader("Link", link));
            }

            if (testCase.contentType != null) {
                mockResponseBuilder.withHeader("Content-Type", testCase.contentType.toString());
            }

            mockResponseBuilder.withBody(content);

        } else {
            mockResponseBuilder.withStatus(404);
        }

        stubFor(get(urlEqualTo(inputPath.substring(testBase.length()))).willReturn(mockResponseBuilder));
    }

    public void stop() {
        verify(getRequestedFor(urlMatching(testCase.input.toString().substring(testBase.length())))
                .withHeader("accept", equalTo(HttpLoader.getAcceptHeader())));

        if (testCase.redirectTo != null) {
            verify(getRequestedFor(urlMatching(testCase.redirectTo.toString().substring(testBase.length())))
                .withHeader("accept", equalTo(HttpLoader.getAcceptHeader())));
        }
    }
}
