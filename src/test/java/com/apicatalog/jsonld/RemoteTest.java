package com.apicatalog.jsonld;

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
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.loader.HttpDocumentLoader;
import com.apicatalog.jsonld.loader.UrlRewrite;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerJunit;
import com.github.tomakehurst.wiremock.junit.WireMockRule;

@RunWith(Parameterized.class)
public class RemoteTest {

    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    private static final String TESTS_BASE = "https://w3c.github.io";
    
    @Rule
    public final WireMockRule wireMockRule = new WireMockRule();
    
    @Test
    public void testRemote() {

        String inputPath;
        
        if (testCase.redirectTo != null) {
            inputPath = testCase.redirectTo.toString();
            
        } else {
            inputPath = testCase.input.toString();
        }
        
        try (InputStream is = getClass().getResourceAsStream(JsonLdManifestLoader.JSON_LD_API_BASE + inputPath.substring(testCase.baseUri.length()))) {

            String inputContent = is != null 
                            ? new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
                                  .lines()
                                  .collect(Collectors.joining("\n"))
                            : null;
            
            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                if (testCase.redirectTo != null) {
                    stubFor(get(urlEqualTo(testCase.input.toString().substring(TESTS_BASE.length())))
                            .willReturn(aResponse()
                                .withStatus(testCase.httpStatus)
                                .withHeader("Location", testCase.redirectTo.toASCIIString().substring(TESTS_BASE.length()))
                                    )); 
                }
                
                if (inputContent != null) {
                    stubFor(get(urlEqualTo(inputPath.substring(TESTS_BASE.length())))
                          .willReturn(aResponse()
                              .withStatus(200)
                              .withHeader("Content-Type", testCase.contentType)
                              .withBody(inputContent)
                                  ));
                } else {
                    stubFor(get(urlEqualTo(inputPath.substring(TESTS_BASE.length())))
                            .willReturn(aResponse()
                                .withStatus(404)
                                    ));                    
                }

                JsonLdOptions expandOptions = new JsonLdOptions(options);
                expandOptions.setDocumentLoader(new UrlRewrite(TESTS_BASE, wireMockRule.baseUrl(), new HttpDocumentLoader()));
                
                return JsonLd.expand(testCase.input).options(expandOptions).get();
            });

            verify(getRequestedFor(urlMatching(testCase.input.toString().substring(TESTS_BASE.length())))
                    .withHeader("accept", equalTo(HttpDocumentLoader.ACCEPT_HEADER)));

            if (testCase.redirectTo != null) {
                verify(getRequestedFor(urlMatching(testCase.redirectTo.toString().substring(TESTS_BASE.length())))
                        .withHeader("accept", equalTo(HttpDocumentLoader.ACCEPT_HEADER)));                
            }
            
        } catch (JsonLdError e) {

            Assert.fail(e.getMessage());
            
        } catch (IOException e) {
            Assert.fail(e.getMessage());
        }        
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "remote-doc-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
