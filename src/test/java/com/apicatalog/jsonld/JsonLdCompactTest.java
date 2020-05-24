package com.apicatalog.jsonld;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.json.JsonValue;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

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
    public void testExpand() throws IOException, JsonLdError {

        // skip specVersion == 1.0
        //assumeFalse(Version.V1_0.equals(testCase.options.specVersion));
        
        // skip normative == false
        //assumeTrue(testCase.options.normative == null || testCase.options.normative);
        
        JsonLdOptions options = testCase.getOptions();

        JsonValue result = null;

//        try {
//            
//            result = null;JsonLd.createProcessor().compact(URI.create(baseUri + testCase.input), options);
//            
//            if (testCase.expectErrorCode != null) {
//                Assert.fail("expected '" + testCase.expectErrorCode + "' error code");
//            }
//            
//            Assert.assertNotNull(result);
//            
//        } catch (JsonLdError e) {    
////            e.printStackTrace();
//            Assert.assertEquals(testCase.expectErrorCode, e.getCode().name());
//            return;
//        }

        Assert.assertNull(testCase.expectErrorCode);
        
        RemoteDocument expectedDocument = options.getDocumentLoader().loadDocument(URI.create(baseUri + testCase.expect), new LoadDocumentOptions());
        
//        Map<String, Object> properties = new HashMap<>(1);
//        properties.put(JsonGenerator.PRETTY_PRINTING, true);
//
//        JsonWriterFactory writerFactory = Json.createWriterFactory(properties);

//            JsonWriter jsonWriter2 = writerFactory.createWriter(System.out);
//            jsonWriter2.write(result);
//            jsonWriter2.close();
            
        Assert.assertNotNull(expectedDocument);
        Assert.assertNotNull(expectedDocument.getDocument());
        
        // compare expected with the result        
        Assert.assertEquals(expectedDocument.getDocument().asJsonStructure(), result);
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
