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
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.loader.ZipResourceLoader;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.NQuadsReaderException;
import com.apicatalog.rdf.io.nquad.NQuadsWriterException;

@RunWith(Parameterized.class)
public class JsonLdToRdfTest {
    
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

        // blank nodes as predicates are not supported - wont'fix
        assumeFalse("#te075".equals(testCase.id));
        // invalid IRI/URI are not accepted - wont'fix
        assumeFalse("#tli12".equals(testCase.id));
        
        
        Assert.assertNotNull(testCase.baseUri);
        Assert.assertNotNull(testCase.input);

        JsonLdOptions options = testCase.getOptions();
        
        Assert.assertNotNull(options);
        Assert.assertNotNull(options.getDocumentLoader());
        
        RdfDataset result = null;

        try {
  
            result = JsonLd.toRdf(testCase.input).options(options).get();
            
            Assert.assertNotNull("A result is expected but got null.", result);
        
        } catch (JsonLdError e) {
            Assert.assertEquals(testCase.expectErrorCode, e.getCode());
            return;
        }

        Assert.assertTrue("Error code [" + testCase.expectErrorCode + "] is expected but no execption has been thrown. ", testCase.expectErrorCode == null);
        
        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (testCase.expect == null && testCase.type.contains("jld:PositiveSyntaxTest")) {
            return;
        }
        
        Assert.assertNotNull("Test case does not define expected output nor expected error code.", testCase.expect);

        try {
            RemoteDocument inputDocument = (new ZipResourceLoader(false)).loadDocument(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE + testCase.expect.toString().substring("https://w3c.github.io/json-ld-api/tests/".length())), new LoadDocumentOptions());
        
            RdfDataset expected = Rdf.createReader(new ByteArrayInputStream(inputDocument.getDocument().getRawPayload()), RdfFormat.N_QUADS).readDataset();

            Assert.assertNotNull(expected);

            boolean match = RdfComparison.equals(expected, result);

            if (!match) {
                System.out.println("Test " + testCase.id + ": " + testCase.name);
                System.out.println("Expected:");
                
                Rdf.createWriter(System.out, RdfFormat.N_QUADS).write(expected);
    
                System.out.println();
                System.out.println("Actual:");
            
                Rdf.createWriter(System.out, RdfFormat.N_QUADS).write(result);
                
                System.out.println();
            }

            Assert.assertTrue("The result does not match expected output.", match);
            
        } catch (NQuadsReaderException | NQuadsWriterException | JsonLdError | UnsupportedFormatException e ) {
            Assert.fail(e.getMessage());
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError {
        return JsonLdManifestLoader
                    .load(JsonLdManifestLoader.JSON_LD_API_BASE, "toRdf-manifest.jsonld")
                    .stream()            
                    .map(o -> new Object[] {o, o.id, o.name, o.baseUri})
                    .collect(Collectors.toList());
    }
}
