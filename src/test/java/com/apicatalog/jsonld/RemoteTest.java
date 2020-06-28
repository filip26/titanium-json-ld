package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;

import java.net.http.HttpClient;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdMockServer;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerJunit;
import com.apicatalog.jsonld.suite.loader.UriBaseRewriter;
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
    
    public static final String TESTS_BASE = "https://w3c.github.io";
    
    @Rule
    public final WireMockRule wireMockRule = new WireMockRule();

    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();

    @Test
    public void testRemote() {

        // skip, HTML extraction is not supported yet
        assumeFalse("#t0013".equals(testCase.id));
        
        try {

            JsonLdMockServer server = new JsonLdMockServer(testCase, TESTS_BASE);
            
            server.start();
            
            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {
                
                JsonLdOptions expandOptions = new JsonLdOptions(options);
                
                expandOptions.setDocumentLoader(
                                    new UriBaseRewriter(
                                                TESTS_BASE, 
                                                wireMockRule.baseUrl(),
                                                new HttpLoader(HTTP_CLIENT, HttpLoader.MAX_REDIRECTIONS)));
                
                return JsonDocument.of(JsonLd.expand(testCase.input).options(expandOptions).get());
            });

            server.stop();
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
            
        }        
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "remote-doc-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
