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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdOptions;
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
import com.google.common.base.Objects;

import jakarta.json.Json;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
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

        assertNotNull(testCase.baseUri);
        assertNotNull(testCase.input);

        final JsonLdOptions options = testCase.getOptions();

        assertNotNull(options);
        assertNotNull(options.getDocumentLoader());

        Document result = null;

        try {

            result = method.invoke(options);

            assertNotNull(result, "A result is expected but got null");

        } catch (JsonLdError e) {

            if (Objects.equal(e.getCode(), testCase.expectErrorCode)) {
                return true;
            }

            write(testCase, null, null, e);
            fail("Unexpected error [" + e.getCode() + "]: " + e.getMessage() + ".");
            return false;
        }

        if (testCase.expectErrorCode != null) {
            write(testCase, result.getJsonContent().orElse(null), null, null);
            fail("Expected error [" + testCase.expectErrorCode + "] but got " + result.getContentType() + "].");
            return false;
        }

        return validate(testCase, options, result);
    }

    private boolean validate(final JsonLdTestCase testCase, final JsonLdOptions options, final Document result) {

        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (result.getRdfContent().isPresent() && testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
            return true;
        }

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new DocumentLoaderOptions());

            assertNotNull(expectedDocument);

            // compare expected with the result

            if (expectedDocument.getJsonContent().isPresent()) {

                assertTrue(result.getJsonContent().isPresent(), "Expected JSON document but was " + result.getContentType());

                return compareJson(testCase, result.getJsonContent().get(), expectedDocument.getJsonContent().get());

            } else if (expectedDocument.getRdfContent().isPresent()) {

                assertTrue(result.getRdfContent().isPresent(), "Expected RDF document but was " + result.getContentType());

                return compareRdf(testCase, result.getRdfContent().get(), expectedDocument.getRdfContent().get());
            }

            assertTrue(result.getRdfContent().isPresent(), "Expected " + expectedDocument.getContentType() + " document but was " + result.getContentType());

        } catch (JsonLdError e) {
            fail(e.getMessage());
        }
        return false;
    }

    public static final boolean compareJson(final JsonLdTestCase testCase, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        write(testCase, result, expected, null);

        fail("Expected " + expected + ", but was" + result);
        return false;
    }

    public static void write(final JsonLdTestCase testCase, final JsonStructure result, final JsonStructure expected, JsonLdError error) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id + ": " + testCase.name);

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));

            if (expected != null) {
                write(writer, writerFactory, "Expected", expected);
                writer.println();

            } else if (testCase.expectErrorCode != null) {
                writer.println("Expected: " + testCase.expectErrorCode);
            }

            if (result != null) {
                write(writer, writerFactory, "Actual", result);
                writer.println();
            }
            if (error != null) {
                writer.println("Actual: ");
                error.printStackTrace(writer);
            }
        }

        System.out.println(stringWriter.toString());
    }

    public static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }

    public static final boolean compareRdf(final JsonLdTestCase testCase, final RdfDataset result, final RdfDataset expected) {

        try {

            boolean match = RdfComparison.equals(expected, result);

            if (!match) {

                final StringWriter stringWriter = new StringWriter();

                try (final PrintWriter writer = new PrintWriter(stringWriter)) {
                    writer.println("Test " + testCase.id + ": " + testCase.name);
                    writer.println("Expected:");

                    Rdf.createWriter(MediaType.N_QUADS, writer).write(expected);

                    writer.println();
                    writer.println("Actual:");

                    Rdf.createWriter(MediaType.N_QUADS, writer).write(result);

                    writer.println();

                }

                System.out.print(stringWriter.toString());
            }

            assertTrue(match, "The result does not match expected output.");

            return match;

        } catch (RdfWriterException | UnsupportedContentException | IOException e ) {
            fail(e.getMessage());
        }
        return false;
    }

}