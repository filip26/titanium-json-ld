package com.apicatalog.jsonld.suite;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;

import java.net.URI;

import org.junit.Assert;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.http.link.Link;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.suite.loader.ZipResourceLoader;
import com.apicatalog.jsonld.uri.UriResolver;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;

public final class JsonLdMockServer {

    private final JsonLdTestCase testCase;
    private final String testBase;
    
    public JsonLdMockServer(JsonLdTestCase testCase, String testBase) {
        this.testCase = testCase;
        this.testBase = testBase;
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
                
                Assert.assertNotNull(contentType);

                String linkUri = UriResolver.resolve(testCase.input, link.target().toString());

                RemoteDocument linkedDocument = (new ZipResourceLoader(false)).loadDocument(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE +  linkUri.substring(testCase.baseUri.length())), new LoadDocumentOptions());

                Assert.assertNotNull(linkedDocument);
                Assert.assertNotNull(linkedDocument.getDocument());

                linkedDocument.getDocument().getRawPayload().ifPresent(byteArray -> {
                    
                    stubFor(get(urlEqualTo(linkUri.substring(testBase.length())))
                            .willReturn(aResponse()
                                .withStatus(200)
                                .withHeader("Content-Type", contentType.toString())
                                .withBody(byteArray))
                                    );                    
                });                
            }
                
            ResponseDefinitionBuilder mockResponseBuilder = aResponse();

            RemoteDocument inputDocument = (new ZipResourceLoader(false)).loadDocument(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE + inputPath.substring(testCase.baseUri.length())), new LoadDocumentOptions());
            
            if (inputDocument != null) {
                mockResponseBuilder.withStatus(200);
                
                if (testCase.httpLink != null) {
                    testCase.httpLink.forEach(link -> mockResponseBuilder.withHeader("Link", link));
                }
                                
                inputDocument.getDocument().getRawPayload().ifPresent(byteArray -> {
                            mockResponseBuilder
                                    .withHeader("Content-Type", testCase.contentType.toString())
                                    .withBody(byteArray);
                });
                
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
