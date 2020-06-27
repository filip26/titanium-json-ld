package com.apicatalog.jsonld.suite;

import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonStructure;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.stream.JsonGenerator;

import org.junit.Assert;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

public class JsonLdTestRunnerJunit {

    private final JsonLdTestCase testCase;
    
    public JsonLdTestRunnerJunit(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }
    
    public void execute(JsonLdTestCaseMethod method) throws JsonLdError {

        Assert.assertNotNull(testCase.baseUri);
        Assert.assertNotNull(testCase.input);

        JsonLdOptions options = testCase.getOptions();
        
        Assert.assertNotNull(options);
        Assert.assertNotNull(options.getDocumentLoader());
        
        Document result = null;
        
        try {
  
            result = method.invoke(options);
            
            Assert.assertNotNull("A result is expected but got null", result);
        
        } catch (JsonLdError e) {
            Assert.assertEquals(e.getMessage(), testCase.expectErrorCode, e.getCode());
            return;
        }
        
        Assert.assertNull(testCase.expectErrorCode);
        Assert.assertNotNull(testCase.expect);
        
        Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new LoadDocumentOptions());
                    
        Assert.assertNotNull(expectedDocument);

        // compare expected with the result
        
        if (expectedDocument.getJsonContent().isPresent()) {
            
            Assert.assertTrue("Expected JSON document but got " + result.getContentType(), result.getJsonContent().isPresent());
            
            compareJson(result.getJsonContent().get(), expectedDocument.getJsonContent().get());
            return;
        }
        
        Assert.assertTrue("Expected RDF document but got " + result.getContentType(), result.getRdfContent().isPresent());
    }

    public void compareJson(JsonStructure result, JsonStructure expectedDocument) {

        if (JsonLdComparison.equals(expectedDocument, result)) {
            return;
        }
        System.out.println("Test " + testCase.id + ": " + testCase.name);
        System.out.println("Expected:");

        Map<String, Object> properties = new HashMap<>(1);
        properties.put(JsonGenerator.PRETTY_PRINTING, true);

        JsonWriterFactory writerFactory = Json.createWriterFactory(properties);

        StringWriter writer = new StringWriter();
        
        JsonWriter jsonWriter1 = writerFactory.createWriter(writer);
        jsonWriter1.write(expectedDocument);
        jsonWriter1.close();

        writer.append("\n\n");
        writer.append("Actual:\n");

        JsonWriter jsonWriter2 = writerFactory.createWriter(writer);
        jsonWriter2.write(result);
        jsonWriter2.close();

        System.out.print(writer.toString());
        System.out.println();
        System.out.println();
        Assert.fail("Expected " + expectedDocument + ", but was" + result);
    }    
}