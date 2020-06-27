package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
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
import com.apicatalog.jsonld.suite.loader.ZipResourceLoader;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.NQuadsReaderException;

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
        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        //assumeTrue(testCase.options.normative == null || testCase.options.normative);

        try {
            
            (new JsonLdTestRunnerJunit(testCase)).execute(options -> {

                try {            
                    byte[] content = (new ZipResourceLoader()).fetchBytes(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE + testCase.input.toString().substring("https://w3c.github.io/json-ld-api/tests/".length())));

                    Assert.assertNotNull(content);
                    
                    RdfDataset input = Rdf.createReader(new ByteArrayInputStream(content), RdfFormat.N_QUADS).readDataset();
                    
                    return JsonLd.fromRdf(input).options(options).get();
                
                } catch (IOException | NQuadsReaderException | UnsupportedFormatException e) {
                    Assert.fail(e.getMessage());
                }
                
                return null;
                
            });
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "fromRdf-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
