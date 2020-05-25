package com.apicatalog.jsonld;

import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.grammar.Version;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;

@RunWith(Parameterized.class)
public class CompactTest {

    @Parameterized.Parameter(0)
    public JsonLdTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;
        
    @Parameterized.Parameter(3)
    public String baseUri;
    
    @Test
    public void testCompact() {

        // skip specVersion == 1.0
        assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        assumeTrue(testCase.options.normative == null || testCase.options.normative);
        
        Assert.assertNotNull(testCase.context);
        
        try {
            testCase.execute(options -> {
                
                //pre-load context
                RemoteDocument jsonContext = options.getDocumentLoader().loadDocument(testCase.context, new LoadDocumentOptions());
                
                Assert.assertNotNull(jsonContext);
                Assert.assertNotNull(jsonContext.getDocument());
                Assert.assertNotNull(jsonContext.getDocument().asJsonStructure());
                                
                return JsonLd
                            .createProcessor()
                            .compact(
                                    testCase.input, 
                                    JsonLdContext.of(jsonContext.getDocument().asJsonStructure()),
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
