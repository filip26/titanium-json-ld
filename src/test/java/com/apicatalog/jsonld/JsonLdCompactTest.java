package com.apicatalog.jsonld;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;

@RunWith(Parameterized.class)
public class JsonLdCompactTest {

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
        
        Assert.assertNotNull(testCase.context);
        
        try {
            testCase.execute(options -> {
                return JsonLd
                            .createProcessor()
                            .compact(
                                    URI.create(testCase.baseUri + testCase.input), 
                                    JsonLdContext.of(URI.create(testCase.baseUri + testCase.context)),
                                    options
                                    );
            });
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
        }            
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException {        
        return JsonLdManifestLoader
                .load("compact-manifest.jsonld")
                .stream()            
                .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                .collect(Collectors.toList());
    }
}
