package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.stream.Collectors;

import org.apache.commons.rdf.api.Dataset;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.rdf.Rdf;

@RunWith(Parameterized.class)
public class ToRdfTest {
    
    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testToRdf() throws IOException {

        // skip specVersion == 1.0
        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        //assumeTrue(testCase.options.normative == null || testCase.options.normative);

        
        Assert.assertNotNull(testCase.baseUri);
        Assert.assertNotNull(testCase.input);

        JsonLdOptions options = testCase.getOptions();
        
        Assert.assertNotNull(options);
        Assert.assertNotNull(options.getDocumentLoader());
        
        Dataset result = null;
        
        try {
  
            result = JsonLd.toRdf(testCase.input).options(options).get();

            
            Assert.assertNotNull("A result is expected but got null", result);
        
        } catch (JsonLdError e) {
            Assert.assertEquals(testCase.expectErrorCode, e.getCode());
            return;
        }
        
        Assert.assertNull(testCase.expectErrorCode);
        Assert.assertNotNull(testCase.expect);
        
        try (InputStream is = getClass().getResourceAsStream(testCase.expect.toString().substring("classpath:".length()))) {

            Dataset expected = Rdf.createParser(new InputStreamReader(is)).getDataset();

            Assert.assertNotNull(expected);

            //TODO compare expected with result with
            // https://www.w3.org/TR/rdf11-concepts/#dfn-dataset-isomorphism
            Assert.assertEquals(expected, result);
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException {
        return JsonLdManifestLoader
                    .load("toRdf-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
