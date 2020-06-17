package com.apicatalog.jsonld.suite;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.http.Link;
import com.apicatalog.jsonld.http.MediaType;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.uri.UriResolver;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;

public final class JsonLdMockServer {

    private final JsonLdTestCase testCase;
    private final String testBase;
    
    public JsonLdMockServer(JsonLdTestCase testCase, String testBase) {
        this.testCase = testCase;
        this.testBase = testBase;
    }
    
    public void start() throws IOException {
        
        String inputPath;
        
        if (testCase.redirectTo != null) {
            inputPath = testCase.redirectTo.toString();
            
        } else {
            inputPath = testCase.input.toString();
        }

        try (InputStream is = getClass().getResourceAsStream(JsonLdManifestLoader.JSON_LD_API_BASE + inputPath.substring(testCase.baseUri.length()))) {

            if (testCase.redirectTo != null) {
                stubFor(get(urlEqualTo(testCase.input.toString().substring(testBase.length())))
                        .willReturn(aResponse()
                            .withStatus(testCase.httpStatus)
                            .withHeader("Location", testCase.redirectTo.toASCIIString().substring(testBase.length()))
                                )); 
            }
            
            if (testCase.httpLink != null && testCase.httpLink.size() == 1) {
                
                String linkValue = testCase.httpLink.iterator().next();
                
                Link link = Link.valueOf(linkValue, URI.create(".")).iterator().next();
                
                String contentType = link.type();
                
                if (contentType == null || contentType.isBlank()) {
                    if (link.uri().toString().endsWith(".html")) {
                        contentType = MediaType.HTML.toString();
                        
                    } else if (link.uri().toString().endsWith(".jsonld")) {
                        contentType = MediaType.JSON_LD.toString();
                        
                    } else if (link.uri().toString().endsWith(".json")) {
                        contentType = MediaType.JSON.toString();
                    }
                }

                String linkUri = UriResolver.resolve(testCase.input, link.uri().toString());
                
                try (InputStream lis = getClass().getResourceAsStream(JsonLdManifestLoader.JSON_LD_API_BASE + linkUri.substring(testCase.baseUri.length()))) {

                    String inputContent = new BufferedReader(new InputStreamReader(lis, StandardCharsets.UTF_8))
                            .lines()
                            .collect(Collectors.joining("\n"));

                    stubFor(get(urlEqualTo(linkUri.substring(testBase.length())))
                            .willReturn(aResponse()
                                .withStatus(200)
                                .withHeader("Content-Type", contentType)
                                .withBody(inputContent)
                                    )); 
                } 
            }
                
            ResponseDefinitionBuilder mockResponseBuilder = aResponse();
            
            if (is != null) {
                mockResponseBuilder.withStatus(200);
                
                if (testCase.httpLink != null) {
                    testCase.httpLink.forEach(link -> mockResponseBuilder.withHeader("Link", link));
                }
                
                String inputContent = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
                                              .lines()
                                              .collect(Collectors.joining("\n"));
                
                mockResponseBuilder
                        .withHeader("Content-Type", testCase.contentType.toString())
                        .withBody(inputContent);
                
            } else {
                mockResponseBuilder.withStatus(404);                  
            }
                
            stubFor(get(urlEqualTo(inputPath.substring(testBase.length()))).willReturn(mockResponseBuilder));
        }
        
    }
    
    public void stop() {
        verify(getRequestedFor(urlMatching(testCase.input.toString().substring(testBase.length())))
                .withHeader("accept", equalTo(HttpLoader.ACCEPT_HEADER)));

        if (testCase.redirectTo != null) {
            verify(getRequestedFor(urlMatching(testCase.redirectTo.toString().substring(testBase.length())))
                    .withHeader("accept", equalTo(HttpLoader.ACCEPT_HEADER)));                
        }
    }
}
