package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerJunit;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.api.Rdf;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.nquad.NQuadsReaderError;

@RunWith(Parameterized.class)
public class RdfToJsonLdTest {
    
    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testExpand() {

        // skip specVersion == 1.0
        //assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        //assumeTrue(testCase.options.normative == null || testCase.options.normative);

        try {
            
            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {
                
                try (InputStream is = getClass().getResourceAsStream(JsonLdManifestLoader.RESOURCES_BASE + testCase.input.toString().substring("https://w3c.github.io/json-ld-api/tests/".length()))) {
            
                    Assert.assertNotNull(is);
                    
                    RdfDataset input = Rdf.createReader(is, RdfFormat.N_QUADS).readDataset();
                    
                    return JsonLd.fromRdf(input).options(options).get();
                
                } catch (IOException | NQuadsReaderError e) {
                    Assert.fail(e.getMessage());
                }
                
                return null;
                
            });
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException {
        return JsonLdManifestLoader
                    .load("fromRdf-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
