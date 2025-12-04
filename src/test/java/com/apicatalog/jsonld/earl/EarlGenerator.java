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
package com.apicatalog.jsonld.earl;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import com.apicatalog.jsonld.JakartaTestSuite;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.SuiteEvironment;
import com.apicatalog.jsonld.loader.LoaderException;
import com.apicatalog.jsonld.loader.UriRewriter;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.jsonld.test.EarlRunner;
import com.apicatalog.jsonld.test.MockServer;
import com.apicatalog.jsonld.test.TestCase;
import com.apicatalog.jsonld.test.TestManifest;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.flow.QuadEmitter;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;

public class EarlGenerator {

    public static final String FILE_NAME = "titanium-json-ld-earl.ttl";
    public static final String VERSION = "2.0.0";
    public static final String RELEASE_DATE = "2025-11-24";

    public static void main(String[] args) throws JsonLdException, IOException {

        SuiteEvironment.LOADER = new ZipResourceLoader(JakartaTestSuite.PARSER);

        (new EarlGenerator()).generate(Paths.get(FILE_NAME));
    }

    public void generate(final Path path) throws JsonLdException, IOException {
        try (final var writer = new PrintWriter(path.toFile())) {
            printHeader(writer);
            testCompact(writer);
            testExpand(writer);
            testFlatten(writer);
            testToRdf(writer);
            testRemote(writer);
            testFromRdf(writer);
            testFrame(writer);
        }
    }

    public void testExpand(PrintWriter writer) throws JsonLdException {
        TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "expand-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        (new EarlRunner(testCase))
                                .execute(options -> JsonLd.expand(testCase.input, options))));
    }

    public void testCompact(final PrintWriter writer) throws JsonLdException {
        TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "compact-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        new EarlRunner(testCase).execute(options -> JsonLd.compact(
                                testCase.input,
                                testCase.context,
                                options))));
    }

    public void testFlatten(final PrintWriter writer) throws JsonLdException {

        TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "flatten-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        new EarlRunner(testCase).execute(options -> JsonLd
                                .flatten(
                                        testCase.input,
                                        testCase.context,
                                        options))));
    }

    public void testToRdf(final PrintWriter writer) throws JsonLdException {

        TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "toRdf-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        (new EarlRunner(testCase)).execute(options -> {
                            final var set = new OrderedQuadSet();
                            JsonLd.toRdf(testCase.input, new QuadAcceptor(set), options);
                            return set;
                        })));
    }

    public void testFromRdf(PrintWriter writer) {

        TestManifest
                .load(
                        TestManifest.JSON_LD_API_BASE,
                        "fromRdf-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        (new EarlRunner(testCase)).execute(options -> {

                            final var toLd = JsonLd
                                    .fromRdf(options)
                                    .jsonParser(JakartaTestSuite.PARSER);

                            try {
                                QuadEmitter.create(toLd).emit(
                                        testCase.readQuads(
                                                ZipResourceLoader
                                                        .fetchBytes(testCase.rebase(testCase.input))));
                                return toLd.toJsonLd();

                            } catch (NQuadsReaderException | LoaderException e) {
                                throw new IllegalStateException(e);
                            }
                        })));
    }

    public void testFrame(PrintWriter writer) throws JsonLdException {
        TestManifest
                .load(
                        TestManifest.JSON_LD_FRAMING_BASE,
                        "frame-manifest.jsonld",
                        SuiteEvironment.LOADER)
                .stream()
                .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                .forEach(testCase -> printResult(writer, testCase.uri,
                        (new EarlRunner(testCase)).execute(options -> JsonLd.frame(testCase.input, testCase.frame, options))));
    }

    public void testRemote(PrintWriter writer) throws JsonLdException {

        try (var server = new MockServer(TestManifest.TESTS_BASE, TestManifest.JSON_LD_API_BASE)) {

            server.start();

            TestManifest
                    .load(
                            TestManifest.JSON_LD_API_BASE,
                            "remote-doc-manifest.jsonld",
                            SuiteEvironment.LOADER)
                    .stream()
                    .filter(TestCase.IS_NOT_V1_0) // skip specVersion == 1.0
                    .forEach(testCase -> {

                        boolean result = false;

                        try {
                            server.setup(testCase);

                            result = (new EarlRunner(testCase)).execute(options -> {

                                final var expandOptions = Options.copyOf(options);

                                expandOptions.loader(
                                        UriRewriter.newBuilder(JakartaTestSuite.HTTP_LOADER)
                                                .rebase(TestManifest.TESTS_BASE,
                                                        server.baseUrl())
                                                .build());

                                return JsonLd.expand(testCase.input, expandOptions);
                            });

                        } catch (LoaderException e) {
                            e.printStackTrace();
                            result = false;
                        }

                        printResult(writer, testCase.uri, result);
                    });
        }
    }

    void printResult(PrintWriter writer, String testUri, boolean passed) {

        if (!passed) {
            System.out.println("Failed: " + testUri);
        }

        var timestamp = DateTimeFormatter.ISO_INSTANT
                .format(Instant.now().truncatedTo(ChronoUnit.SECONDS));

        String report = """
                [ a earl:Assertion ;
                  earl:assertedBy <https://github.com/filip26> ;
                  earl:subject <https://github.com/filip26/titanium-json-ld> ;
                  earl:test <%s> ;
                  earl:result [
                    a earl:TestResult ;
                    earl:outcome %s ;
                    dc:date "%s"^^xsd:dateTime
                  ] ;
                  earl:mode earl:automatic
                ] .
                """.formatted(
                testUri,
                passed ? "earl:passed" : "earl:failed",
                timestamp);

        writer.println(report);
    }

    void printHeader(PrintWriter writer) {

        var timestamp = DateTimeFormatter.ISO_INSTANT
                .format(Instant.now().truncatedTo(ChronoUnit.SECONDS));

        String prefixes = """
                @prefix dc: <http://purl.org/dc/terms/> .
                @prefix doap: <http://usefulinc.com/ns/doap#> .
                @prefix foaf: <http://xmlns.com/foaf/0.1/> .
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                @prefix earl: <http://www.w3.org/ns/earl#> .
                """;

        String primaryTopic = """
                <> foaf:primaryTopic <https://github.com/filip26/titanium-json-ld> ;
                  dc:issued "%s"^^xsd:dateTime ;
                  foaf:maker <https://github.com/filip26> .
                """.formatted(timestamp);

        String project = """
                <https://github.com/filip26/titanium-json-ld> a earl:TestSubject, doap:Project, earl:Software ;
                  dc:title "Titanium" ;
                  dc:creator <https://github.com/filip26> ;
                  doap:name "Titanium" ;
                  doap:description "A JSON-LD 1.1 Processor & API for Java" ;
                  doap:developer <https://github.com/filip26> ;
                  doap:homepage <https://github.com/filip26/titanium-json-ld> ;
                  doap:license <https://github.com/filip26/titanium-json-ld/blob/main/LICENSE> ;
                  doap:release [
                    doap:name "Titanium v%s" ;
                    doap:revision "%s" ;
                    doap:created "%s"^^xsd:date
                  ] ;
                  doap:programming-language "Java" .
                """.formatted(VERSION, VERSION, RELEASE_DATE);

        String person = """
                <https://github.com/filip26> a earl:Assertor, foaf:Person ;
                  foaf:name "Filip Kolařík" ;
                  foaf:homepage <https://github.com/filip26> .
                """;

        writer.println(prefixes);
        writer.println(primaryTopic);
        writer.println(project);
        writer.println(person);
    }
}
