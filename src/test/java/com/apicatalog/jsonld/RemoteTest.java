package com.apicatalog.jsonld;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
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
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.loader.UrlConnectionLoader;
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
    
    @Rule
    public final WireMockRule wireMockRule = new WireMockRule();
    
    @Test
    public void testRemote() {

        String inputPath = testCase.input.toString().substring(JsonLdManifestLoader.JSON_LD_API_BASE.length() -1);
        
        try (InputStream is = getClass().getResourceAsStream(JsonLdManifestLoader.JSON_LD_API_BASE + inputPath.substring("/tests/".length()))) {
            
            Assert.assertNotNull(is);

            String inputContent = 
                        new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
                              .lines()
                              .collect(Collectors.joining("\n"));
            
            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                stubFor(get(urlEqualTo(testCase.input.toString().substring("https://w3c.github.io".length())))
                      .willReturn(aResponse()
                          .withStatus(200)
                          .withHeader("Content-Type", testCase.contentType)
                          .withBody(inputContent)
                              ));

                JsonLdOptions expandOptions = new JsonLdOptions(options);
                expandOptions.setDocumentLoader(new UrlRewrite("https://w3c.github.io", wireMockRule.baseUrl(), new UrlConnectionLoader()));
                
                return JsonLd.expand(testCase.input).options(expandOptions).get();                        
            });
        
        } catch (JsonLdError e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
            
        } catch (IOException e) {
            Assert.fail(e.getMessage());
        }

        
//        Result result = myHttpServiceCallingObject.doSomething();

//        assertTrue(result.wasSuccessful());

//        verify(postRequestedFor(urlMatching("/my/resource/[a-z0-9]+"))
//                .withRequestBody(matching(".*<message>1234</message>.*"))
//                .withHeader("Content-Type", notMatching("application/json")));
        
//        // skip specVersion == 1.0
//        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
//        
//        try {
//            (new JsonLdTestRunnerJunit(testCase)).execute(options ->
//            
//                        JsonLd.expand(testCase.input).options(options).get()
//                        
//            );
//            
//        } catch (JsonLdError e) {
//            Assert.fail(e.getMessage());
//        }
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
