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
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.apicatalog.jcs.Jcs;
import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderException;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.jsonld.test.TestCase.Type;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.model.RdfQuad;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.nquads.NQuadsWriter;
import com.apicatalog.rdf.provider.QuadEmitter;
import com.apicatalog.rdf.provider.QuadSetProvider;
import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.jakarta.JakartaEmitter;

import jakarta.json.Json;
import jakarta.json.JsonStructure;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonGeneratorFactory;

public class JunitRunner {

    public static final JsonGeneratorFactory JSON_GENERATOR_FACTORY = Json.createGeneratorFactory(
            Map.of(JsonGenerator.PRETTY_PRINTING, true));

    private final TestCase testCase;

    public JunitRunner(TestCase testCase) {
        this.testCase = testCase;
    }

    public boolean execute() {

        if (testCase.type.contains(Type.COMPACT_TEST)) {
            return execute(
                    options -> JsonLd.compact(
                            testCase.input,
                            testCase.context,
                            options));
        }

        if (testCase.type.contains(Type.EXPAND_TEST)) {
            return execute(
                    options -> JsonLd.expand(
                            testCase.input,
                            options));
        }

        if (testCase.type.contains(Type.FLATTEN_TEST)) {
            return execute(
                    options -> JsonLd.flatten(
                            testCase.input,
                            testCase.context,
                            options));
        }

        if (testCase.type.contains(Type.TO_RDF_TEST)) {
            return execute(options -> {

                final var set = new LinkedHashSet<RdfQuad>();

                options.rdfJsonLiteralWriter(json -> new String(Jcs.canonize(json), StandardCharsets.UTF_8));

                JsonLd.toRdf(testCase.input, new QuadSetProvider(set)::quad, options);

                return set;
            });
        }

        if (testCase.type.contains(Type.FROM_RDF_TEST)) {
            return execute(options -> {

                final var toLd = JsonLd.fromRdf(options).jsonReader(JakartaTestSuite::read);

                try {

                    QuadEmitter.create(toLd::quad).emit(readQuads(testCase, testCase.input));

                } catch (Exception e) {
                    if (toLd.error() != null) {
                        throw toLd.error();
                    }
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

        final var options = testCase.getOptions();

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

            if (testCase.type.contains(TestCase.Type.TO_RDF_TEST)) {
                return validateQuads(testCase, options, (Set<RdfQuad>) result, true);
            }

            return validateJsonLd(
                    testCase,
                    options,
                    result,
                    true);

        } catch (IOException e) {
            fail(e);
            return false;

        } catch (JsonLdException e) {

            if (testCase.expectErrorCode == e.code()
                    || (JsonLdException.ErrorCode.LOADING_DOCUMENT_FAILED == e.code()
                            && testCase.expectErrorCode != null
                            && e.getCause() instanceof LoaderException le
                            && testCase.expectErrorCode.name() == le.code().name())) {
                return true;
            }

            write(testCase, null, null, e);

            if (testCase.expectErrorCode != null) {
                fail("Unexpected error " + e.code() + ", exptected " + testCase.expectErrorCode + ".");

            } else {
                fail("Unexpected error " + e + ".");
            }

            return false;

//        } catch (RdfConsumerException e) {
//
//            if (e.getCause() instanceof JsonLdException) {
//                if (Objects.equals(((JsonLdException) e.getCause()).code(), testCase.expectErrorCode)) {
//                    return true;
//                }
//
//                write(testCase, null, null, ((JsonLdException) e.getCause()));
//                fail("Unexpected error: " + ((JsonLdException) e.getCause()) + ".");
//
//            }
//            return false;
        }
    }

    public static boolean validateJsonLd(
            final TestCase testCase,
            final Options options,
            final Object result,
            final boolean fail) throws IOException {

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            var expectedDocument = options.loader().loadDocument(testCase.expect, DocumentLoader.defaultOptions());

            assertNotNull(expectedDocument);

            // compare expected with the result
            return compareJson(testCase, result, expectedDocument.content(), fail);

        } catch (LoaderException e) {
            if (fail) {
                fail(e.getMessage());
            }
        }

        return false;
    }

    public static boolean validateQuads(
            final TestCase testCase,
            final Options options,
            final Set<RdfQuad> result,
            final boolean fail) {

        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (testCase.expect == null && testCase.type.contains(Type.POSITIVE_SYNTAX_TEST)) {
            return true;
        }

        assertNotNull(testCase.expect, "Test case does not define expected output nor expected error code.");

        try {
            // compare expected with the result
            return compareRdf(
                    testCase,
                    result,
                    readQuads(testCase, testCase.expect),
                    fail);

        } catch (LoaderException | NQuadsReaderException | IOException e) {
            if (fail) {
                fail(e.getMessage());
            }
        }

        return false;
    }

    public static final boolean compareJson(
            final TestCase testCase,
            final Object result,
            final Object expected,
            final boolean fail) throws IOException {

        if (Comparison.equals(expected, result)) {
            return true;
        }

        write(testCase,
                result,
                expected,
                null);

        if (fail) {
            fail("Expected " + expected + ", but was" + result);
        }
        return false;
    }

    public static void write(
            TestCase testCase,
            Object result,
            Object expected,
            JsonLdException error) {

        final var stringWriter = new StringWriter();

        try (final var writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase.id + ": " + testCase.name);

            if (expected != null) {
                write(writer, JSON_GENERATOR_FACTORY, "Expected", expected);
                writer.println();

            } else if (testCase.expectErrorCode != null) {
                writer.println("Expected: " + testCase.expectErrorCode);
            }

            if (result != null) {
                write(writer, JSON_GENERATOR_FACTORY, "Actual", result);
                writer.println();
            }
            if (error != null) {
                writer.println("Actual: ");
                error.printStackTrace(writer);
            }
        }

        System.out.println(stringWriter.toString());
    }

    public static final void write(
            PrintWriter writer,
            JsonGeneratorFactory generator,
            String name,
            Object result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (var jsonWriter = new JakartaEmitter(generator.createGenerator(out))) {
            Tree.write(result, jsonWriter);

        } catch (IOException e) {
            fail(e);
        }

        writer.write(out.toString());
        writer.println();
    }

    public static final boolean compareRdf(
            final TestCase testCase,
            final Set<RdfQuad> result,
            final Set<RdfQuad> expected,
            final boolean fail) throws IOException {

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

        if (fail) {
            assertTrue(match, "The result does not match expected output.");
        }

        return match;
    }

    static Set<RdfQuad> readQuads(TestCase testCase, URI uri)
            throws NQuadsReaderException, LoaderException, IOException {

        Set<RdfQuad> set = Set.of();

        final var rebased = testCase.rebase(uri);

        if ("classpath".equals(rebased.getScheme())) {
            try (var is = testCase.getClass().getResourceAsStream(rebased.getPath())) {
                set = testCase.readQuads(is);
            }

        } else if ("zip".equals(rebased.getScheme())) {
            set = testCase.readQuads(ZipResourceLoader.fetchBytes(rebased));
        }

        assertNotNull(set);
        return set;
    }
}