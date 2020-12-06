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
package com.apicatalog.jsonld.test;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Map;

import org.junit.Assert;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.test.JsonLdTestCase.Type;
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

    public boolean execute() {
        
        if (testCase.type.contains(Type.COMPACT_TEST)) {
            return execute(options -> JsonDocument.of(JsonLd.compact(testCase.input, testCase.context).options(options).get()));
        }
        
        if (testCase.type.contains(Type.EXPAND_TEST)) {
            return execute(options -> JsonDocument.of(JsonLd.expand(testCase.input).options(options).get()));
        }
        
        if (testCase.type.contains(Type.FLATTEN_TEST)) {
            return execute(options -> JsonDocument.of(JsonLd.flatten(testCase.input).context(testCase.context).options(options).get()));
        }

        if (testCase.type.contains(Type.TO_RDF_TEST)) {
            return execute(options -> RdfDocument.of(JsonLd.toRdf(testCase.input).options(options).get()));
        }
        
        if (testCase.type.contains(Type.FROM_RDF_TEST)) {
            return execute(options -> JsonDocument.of(JsonLd.fromRdf(testCase.input).options(options).get()));
        }
        
        if (testCase.type.contains(Type.FRAME_TEST)) {
            return execute(options -> JsonDocument.of(JsonLd.frame(testCase.input, testCase.frame).options(options).get()));
        }
        
        throw new IllegalStateException();
    }
    
    public boolean execute(final JsonLdTestCaseMethod method) {

        Assert.assertNotNull(testCase.baseUri);
        Assert.assertNotNull(testCase.input);

        final JsonLdOptions options = testCase.getOptions();

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
        if (result.getRdfContent().isPresent() && testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
            return true;
        }
        
        Assert.assertNotNull("Test case does not define expected output nor expected error code.", testCase.expect);
        
        try {
            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new DocumentLoaderOptions());
                        
            Assert.assertNotNull(expectedDocument);
    
            // compare expected with the result
            
            if (expectedDocument.getJsonContent().isPresent()) {
                
                Assert.assertTrue("Expected JSON document but was " + result.getContentType(), result.getJsonContent().isPresent());
                
                return compareJson(testCase.id + ": " + testCase.name, result.getJsonContent().get(), expectedDocument.getJsonContent().get());
                
            } else if (expectedDocument.getRdfContent().isPresent()) {
                
                Assert.assertTrue("Expected Rdf document but was " + result.getContentType(), result.getRdfContent().isPresent());

                return compareRdf(testCase.id + ": " + testCase.name, result.getRdfContent().get(), expectedDocument.getRdfContent().get());
            }
            
            Assert.assertTrue("Expected " + expectedDocument.getContentType() + " document but was " + result.getContentType(), result.getRdfContent().isPresent());
            
        } catch (JsonLdError e) {
            Assert.fail(e.getMessage());
        }
        return false;
    }

    public static final boolean compareJson(final String testName, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }
        
        final StringWriter stringWriter = new StringWriter();
        
        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testName);
            writer.println("Expected:");
            
            final JsonWriterFactory writerFactory = Json.createWriterFactory(Map.of(JsonGenerator.PRETTY_PRINTING, true));
            
            final StringWriter out1 = new StringWriter();
            
            try (final JsonWriter jsonWriter = writerFactory.createWriter(out1)) {
                jsonWriter.write(expected);
            }

            writer.write(out1.toString());
            writer.println();
            writer.println();
            writer.println("Actual:");
            
            final StringWriter out2 = new StringWriter();

            try (final JsonWriter jsonWriter = writerFactory.createWriter(out2)) {
                jsonWriter.write(result);                
            }

            writer.write(out2.toString());
            writer.println();
            writer.println();
        }
        
        System.out.println(stringWriter.toString());

        Assert.fail("Expected " + expected + ", but was" + result);        
        return false;
    }  
    
    public static final boolean compareRdf(final String testName, final RdfDataset result, final RdfDataset expected) {

        try {

            boolean match = RdfComparison.equals(expected, result);

            if (!match) {
                
                final StringWriter stringWriter = new StringWriter();
                
                try (final PrintWriter writer = new PrintWriter(stringWriter)) {
                    writer.println("Test " + testName);
                    writer.println("Expected:");
                    
                    Rdf.createWriter(MediaType.N_QUADS, writer).write(expected);
        
                    writer.println();
                    writer.println("Actual:");
                
                    Rdf.createWriter(MediaType.N_QUADS, writer).write(result);
                    
                    writer.println();
                    
                }
                
                System.out.print(stringWriter.toString());
            }

            Assert.assertTrue("The result does not match expected output.", match);
            
            return match;
            
        } catch (RdfWriterException | UnsupportedContentException | IOException e ) {
            Assert.fail(e.getMessage());
        }
        return false;
    }
    
}