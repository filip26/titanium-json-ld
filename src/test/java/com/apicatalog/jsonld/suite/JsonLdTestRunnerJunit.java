/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.suite;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Map;

import org.junit.Assert;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.error.RdfWriterException;
import com.apicatalog.rdf.io.error.UnsupportedContentException;

import jakarta.json.Json;
import jakarta.json.JsonStructure;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public class JsonLdTestRunnerJunit {

    private final JsonLdTestCase testCase;
    
    public JsonLdTestRunnerJunit(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }
    
    public boolean execute(JsonLdTestCaseMethod method) {

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
            return true;
        }

        return validate(testCase, options, result);
    }
    
    private boolean validate(final JsonLdTestCase testCase, final JsonLdOptions options, final Document result) {

        Assert.assertNull(testCase.expectErrorCode);

        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (result.getRdfContent().isPresent() && testCase.expect == null && testCase.type.contains("jld:PositiveSyntaxTest")) {
            return true;
        }
        
        Assert.assertNotNull("Test case does not define expected output nor expected error code.", testCase.expect);
        
        try {
            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new DocumentLoaderOptions());
                        
            Assert.assertNotNull(expectedDocument);
    
            // compare expected with the result
            
            if (expectedDocument.getJsonContent().isPresent()) {
                
                Assert.assertTrue("Expected JSON document but was " + result.getContentType(), result.getJsonContent().isPresent());
                
                return compareJson(result.getJsonContent().get(), expectedDocument.getJsonContent().get());
                
            } else if (expectedDocument.getRdfContent().isPresent()) {
                
                Assert.assertTrue("Expected Rdf document but was " + result.getContentType(), result.getRdfContent().isPresent());

                return compareRdf(result.getRdfContent().get(), expectedDocument.getRdfContent().get());
            }
            
            Assert.assertTrue("Expected " + expectedDocument.getContentType() + " document but was " + result.getContentType(), result.getRdfContent().isPresent());
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
        }
        return false;
    }

    private boolean compareJson(JsonStructure result, JsonStructure expectedDocument) {

        if (JsonLdComparison.equals(expectedDocument, result)) {
            return true;
        }
        
        System.out.println("Test " + testCase.id + ": " + testCase.name);
        System.out.println("Expected:");

        JsonWriterFactory writerFactory = Json.createWriterFactory(Map.of(JsonGenerator.PRETTY_PRINTING, true));

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
        
        return false;
    }  
    
    private boolean compareRdf(RdfDataset result, RdfDataset expected) {

        try {

            boolean match = RdfComparison.equals(expected, result);

            if (!match) {
                System.out.println("Test " + testCase.id + ": " + testCase.name);
                System.out.println("Expected:");
                
                Rdf.createWriter(MediaType.N_QUADS, System.out).write(expected);
    
                System.out.println();
                System.out.println("Actual:");
            
                Rdf.createWriter(MediaType.N_QUADS, System.out).write(result);
                
                System.out.println();
            }

            Assert.assertTrue("The result does not match expected output.", match);
            
            return match;
            
        } catch (RdfWriterException | UnsupportedContentException | IOException e ) {
            Assert.fail(e.getMessage());
        }
        return false;
    }
    
}