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
package com.apicatalog.jsonld.test;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.DefaultHttpClient;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.web.link.Link;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;

public final class JsonLdMockServer {

    private final JsonLdTestCase testCase;
    private final String testBase;
    private final String resourceBase;
    private final DocumentLoader loader;

    public JsonLdMockServer(JsonLdTestCase testCase, String testBase, String resourceBase, DocumentLoader loader) {
        this.testCase = testCase;
        this.testBase = testBase;
        this.resourceBase = resourceBase;
        this.loader = loader;
    }

    public void start() throws JsonLdException {

        String inputPath;

        if (testCase.options.redirectTo != null) {
            inputPath = testCase.options.redirectTo.toString();

        } else {
            inputPath = testCase.input.toString();
        }


        if (testCase.options.redirectTo != null) {
            stubFor(get(urlEqualTo(testCase.input.toString().substring(testBase.length())))
                    .willReturn(aResponse()
                        .withStatus(testCase.options.httpStatus)
                        .withHeader("Location", testCase.options.redirectTo.toASCIIString().substring(testBase.length()))
                            ));
        }

        if (testCase.options.httpLink != null && testCase.options.httpLink.size() == 1) {

            String linkValue = testCase.options.httpLink.iterator().next();

            Link link = Link.of(linkValue, URI.create(".")).iterator().next();

            final MediaType contentType;

            if (link.type() != null) {

                contentType = link.type();

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

            byte[] content  = fetchBytes(URI.create(resourceBase +  linkUri.substring(testCase.baseUri.length())));

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

        byte[] content = fetchBytes(URI.create(resourceBase + inputPath.substring(testCase.baseUri.length())));

        if (content != null) {
            mockResponseBuilder.withStatus(200);

            if (testCase.options.httpLink != null) {
                testCase.options.httpLink.forEach(link -> mockResponseBuilder.withHeader("Link", link));
            }

            if (testCase.options.contentType != null) {
                mockResponseBuilder.withHeader("Content-Type", testCase.options.contentType.toString());
            }

            mockResponseBuilder.withBody(content);

        } else {
            mockResponseBuilder.withStatus(404);
        }

        stubFor(get(urlEqualTo(inputPath.substring(testBase.length()))).willReturn(mockResponseBuilder));
    }

    public void stop() {
        verify(getRequestedFor(urlMatching(testCase.input.toString().substring(testBase.length())))
                .withHeader("accept", equalTo(DefaultHttpClient.getAcceptHeader())));

        if (testCase.options.redirectTo != null) {
            verify(getRequestedFor(urlMatching(testCase.options.redirectTo.toString().substring(testBase.length())))
                .withHeader("accept", equalTo(DefaultHttpClient.getAcceptHeader())));
        }
    }
    
    public byte[] fetchBytes(URI url) throws JsonLdException {

        if (!"zip".equals(url.getScheme())) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        URL zipFileUrl = getClass().getResource("/" + url.getAuthority());

        if (zipFileUrl == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }

        File zipFile = null;

        try {
            zipFile = new File(zipFileUrl.toURI());

        } catch (URISyntaxException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }

        try (ZipFile zip = new ZipFile(zipFile)) {

            ZipEntry zipEntry = zip.getEntry(url.getPath().substring(1));

            if (zipEntry == null) {
                return null;
            }

            try (InputStream is = zip.getInputStream(zipEntry)) {

                return readAsByteArray(is);
            }


        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    static final byte[] readAsByteArray(InputStream is) throws IOException {

        final ByteArrayOutputStream byteArrayStream = new ByteArrayOutputStream();

        byte[] buffer = new byte[16384];
        int readed;

        while ((readed = is.read(buffer, 0, buffer.length)) != -1) {
            byteArrayStream.write(buffer, 0, readed);
        }

        return byteArrayStream.toByteArray();
    }
}
