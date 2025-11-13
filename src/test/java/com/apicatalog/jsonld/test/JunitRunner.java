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
import java.net.URI;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.jsonld.test.TestCase.Type;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.nquads.NQuadsWriter;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.flow.QuadEmitter;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.jakarta.JakartaMaterializer;
import com.apicatalog.tree.io.java.NativeAdapter;

import jakarta.json.Json;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public class JunitRunner {

    public static final JsonWriterFactory JSON_WRITER_FACTORY = Json.createWriterFactory(
            Map.of(
                    JsonGenerator.PRETTY_PRINTING,
                    true));

    private final TestCase testCase;

    public JunitRunner(TestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute() {

        if (testCase.type.contains(Type.COMPACT_TEST)) {
            return execute(options -> JsonLd.compact(
                    testCase.input,
                    testCase.context,
                    options));
        }

        if (testCase.type.contains(Type.EXPAND_TEST)) {
            return execute(options -> JsonLd.expand(
                    testCase.input,
                    options));
        }

        if (testCase.type.contains(Type.FLATTEN_TEST)) {
            return execute(options -> JsonLd.flatten(
                    testCase.input,
                    testCase.context,
                    options));
        }

        if (testCase.type.contains(Type.TO_RDF_TEST)) {
            return execute(options -> {

                final var set = new OrderedQuadSet();

                JsonLd.toRdf(testCase.input, new QuadAcceptor(set), options);

                return set;
            });
        }

        if (testCase.type.contains(Type.FROM_RDF_TEST)) {
            return execute(options -> {

                final var toLd = JsonLd.fromRdf(options).jsonParser(JakartaTestSuite.PARSER);

                try {

                    QuadEmitter.create(toLd).emit(readQuads(testCase.input));

                } catch (NQuadsReaderException e) {
                    fail(e);
                }

                return toLd.toJsonLd();
            });
        }

        if (testCase.type.contains(Type.FRAME_TEST)) {
            return execute(options -> JsonLd.frame(testCase.input, testCase.frame, options));
        }

        throw new IllegalStateException("An uknown test type to execute = " + testCase.type + ".");
    }

    public boolean execute(final TestMethod method) {

        assertNotNull(testCase.baseUri);
        assertNotNull(testCase.input);

        final Options options = testCase.getOptions();

        assertNotNull(options);
        assertNotNull(options.loader());

        Object result = null;

        try {

            result = method.invoke(options);

            assertNotNull(result, "A result is expected but got null");

            if (testCase.expectErrorCode != null) {
                if (result instanceof JsonStructure) {
                    write(testCase, (JsonStructure) result, null, null);
                }
                fail("Expected error [" + testCase.expectErrorCode + "] but got " + result + "");
                return false;
            }

            if (result instanceof RdfQuadSet quads) {
                return validateQuads(testCase, options, quads);
            }

            // TODO remove
            if (result instanceof JsonStructure json) {
                return validateJsonLd(testCase, options, json, JakartaAdapter.instance());
            }
//            if (result instanceof JsonNode json) {
//                return validateJsonLd(testCase, options, json, Jackson2Adapter.instance());
//            }

            // System.out.println(" >>>> " + result);
//            if (result instanceof Collection<?> collection) {
            return validateJsonLd(
                    testCase,
                    options,
                    result,
                    NativeAdapter.instance());
//            }

//            fail("Unexpected result type [" + result.getClass() + "]");

//            return false;

        } catch (IOException e) {
            fail(e);
            return false;

        } catch (JsonLdException e) {

            if (Objects.equals(e.code(), testCase.expectErrorCode)) {
                return true;
            }

            write(testCase, null, null, e);

            if (testCase.expectErrorCode != null) {
                fail("Unexpected error " + e.code() + ", exptected " + testCase.expectErrorCode + ".");
            } else {
                fail("Unexpected error " + e + ".");
            }

            return false;

        } catch (RdfConsumerException e) {

            if (e.getCause() instanceof JsonLdException) {
                if (Objects.equals(((JsonLdException) e.getCause()).code(), testCase.expectErrorCode)) {
                    return true;
                }

                write(testCase, null, null, ((JsonLdException) e.getCause()));
                fail("Unexpected error: " + ((JsonLdException) e.getCause()) + ".");

            }
            return false;
        } catch (NQuadsReaderException e) {
            fail(e);
            return false;
        }
    }

    private boolean validateJsonLd(final TestCase testCase, final Options options, final Object result, final TreeAdapter resultAdapter) {

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            var expectedDocument = options.loader().loadDocument(testCase.expect, DocumentLoader.defaultOptions());

            assertNotNull(expectedDocument);

            // compare expected with the result
            return compareJson(testCase, result, resultAdapter, expectedDocument.content());

        } catch (JsonLdException | TreeIOException e) {
            fail(e.getMessage());
        }
        return false;
    }

    private boolean validateQuads(final TestCase testCase, final Options options, final RdfQuadSet result) throws NQuadsReaderException, RdfConsumerException {

        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
            return true;
        }

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            // compare expected with the result
            return compareRdf(testCase,
                    result,
                    readQuads(testCase.expect));

        } catch (JsonLdException | IOException e) {
            fail(e.getMessage());
        }
        return false;
    }

    public static final boolean compareJson(final TestCase testCase, final Object result, final TreeAdapter resultAdapter, final TreeIO expected) throws TreeIOException {

        if (Comparison.equals(expected.node(), expected.adapter(), result, resultAdapter)) {
            return true;
        }

        write(testCase,
                new JakartaMaterializer().node(result, resultAdapter),
                new JakartaMaterializer().node(expected),
                null);

        fail("Expected " + expected.node() + ", but was" + result);
        return false;
    }

    public static void write(final TestCase testCase, final JsonValue result, final JsonValue expected, JsonLdException error) {
        final StringWriter stringWriter = new StringWriter();

        try (final var writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id + ": " + testCase.name);

            if (expected != null) {
                write(writer, JSON_WRITER_FACTORY, "Expected", expected);
                writer.println();

            } else if (testCase.expectErrorCode != null) {
                writer.println("Expected: " + testCase.expectErrorCode);
            }

            if (result != null) {
                write(writer, JSON_WRITER_FACTORY, "Actual", result);
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
    
    public static final boolean compareRdf(final TestCase testCase, final RdfQuadSet result, final RdfQuadSet expected) {

        try {
            boolean match = RdfComparison.equals(expected, result);

            if (!match) {

                final var stringWriter = new StringWriter();

                try (final var writer = new PrintWriter(stringWriter)) {

                    final var emitter = QuadEmitter.create(new NQuadsWriter(writer));

                    writer.println("Test " + testCase.id + ": " + testCase.name);
                    writer.println("Expected:");
                    emitter.emit(expected);

                    writer.println();
                    writer.println("Actual:");
                    emitter.emit(result);

                    writer.println();
                }
                System.out.print(stringWriter.toString());
            }

            assertTrue(match, "The result does not match expected output.");

            return match;

        } catch (RdfConsumerException e) {
            fail(e.getMessage());
        }
        return false;
    }

    RdfQuadSet readQuads(URI uri) throws NQuadsReaderException, RdfConsumerException, JsonLdException, IOException {
        RdfQuadSet set = null;

        final var rebased = testCase.rebase(uri);

        if ("classpath".equals(rebased.getScheme())) {
            try (var is = getClass().getResourceAsStream(rebased.getPath())) {
                set = testCase.readQuads(is);
            }

        } else if ("zip".equals(rebased.getScheme())) {
            set = testCase.readQuads(ZipResourceLoader.fetchBytes(rebased));
        }

        assertNotNull(set);
        return set;
    }
}