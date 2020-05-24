package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.grammar.Version;

@RunWith(Parameterized.class)
public class JsonLdExpandTest {
    
    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testExpand() throws IOException, JsonLdError {

        // skip specVersion == 1.0
        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        assumeTrue(testCase.options.normative == null || testCase.options.normative);

        testCase.execute(() -> {
            return JsonLd.createProcessor().expand(URI.create(testCase.baseUri + testCase.input), testCase.getOptions());
        });
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException {
        return JsonLdManifestLoader
                    .load("expand-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
