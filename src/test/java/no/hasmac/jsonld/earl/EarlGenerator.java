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
package no.hasmac.jsonld.earl;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdOptions;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.document.RdfDocument;
import no.hasmac.jsonld.loader.SchemeRouter;
import no.hasmac.jsonld.loader.UriBaseRewriter;
import no.hasmac.jsonld.loader.ZipResourceLoader;
import no.hasmac.jsonld.test.JsonLdManifestLoader;
import no.hasmac.jsonld.test.JsonLdMockServer;
import no.hasmac.jsonld.test.JsonLdTestCase;
import no.hasmac.jsonld.test.JsonLdTestRunnerEarl;
import com.github.tomakehurst.wiremock.WireMockServer;

import jakarta.json.JsonArray;

public class EarlGenerator {

    public static final String FILE_NAME = "java-jsonp-ld-earl.ttl";
    public static final String VERSION = "0.7";
    public static final String RELEASE_DATE = "2020-06-17";

    public static void main(String[] args) throws JsonLdError, IOException {
        (new EarlGenerator()).generate(Paths.get(FILE_NAME));
    }

    public void generate(final Path path) throws JsonLdError, IOException {

        try (PrintWriter writer = new PrintWriter(path.toFile())) {

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

    public void testExpand(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "expand-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase ->
                        printResult(writer, testCase.uri,
                                (new JsonLdTestRunnerEarl(testCase)).execute(options ->

                                    JsonDocument.of(JsonLd.expand(testCase.input).options(options).get())
                                )
                         )
                    );
    }

    public void testCompact(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "compact-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase ->
                        printResult(writer, testCase.uri,
                             new JsonLdTestRunnerEarl(testCase).execute(options ->

                                    JsonDocument.of(JsonLd.compact(
                                                        testCase.input,
                                                        testCase.context
                                                        )
                                                    .options(options)
                                                    .get())
                                 )
                         )
                    );
    }

    public void testFlatten(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "flatten-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase ->
                        printResult(writer, testCase.uri,
                             new JsonLdTestRunnerEarl(testCase).execute(options ->

                                 JsonDocument.of(JsonLd
                                             .flatten(testCase.input)
                                             .context(testCase.context)
                                             .options(options)
                                             .get())
                                 )
                         )
                    );
    }

    public void testToRdf(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
        .load(JsonLdManifestLoader.JSON_LD_API_BASE, "toRdf-manifest.jsonld", new ZipResourceLoader())
        .stream()
        .forEach(testCase ->
                printResult(writer, testCase.uri,
                        (new JsonLdTestRunnerEarl(testCase)).execute(options ->

                            RdfDocument.of(JsonLd.toRdf(testCase.input).options(options).get())

                        )
                 )
            );
    }

    public void testFromRdf(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "fromRdf-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase ->
                    printResult(writer, testCase.uri,
                            (new JsonLdTestRunnerEarl(testCase)).execute(options ->

                                JsonDocument.of(JsonLd.fromRdf(testCase.input).options(options).get())

                            )
                     )
                );
    }

    public void testFrame(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_FRAMING_BASE, "frame-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase ->
                        printResult(writer, testCase.uri,
                                (new JsonLdTestRunnerEarl(testCase)).execute(options ->

                                    JsonDocument.of(JsonLd.frame(testCase.input, testCase.frame).options(options).get())
                                )
                         )
                    );
    }


    public void testRemote(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "remote-doc-manifest.jsonld", new ZipResourceLoader())
            .stream()
            .forEach(testCase -> {

                boolean result = false;

                try {
                    WireMockServer wireMockServer = new WireMockServer();
                    wireMockServer.start();

                    JsonLdMockServer server = new JsonLdMockServer(testCase, JsonLdTestCase.TESTS_BASE, JsonLdManifestLoader.JSON_LD_API_BASE, new ZipResourceLoader());
                    server.start();


                    result = (new JsonLdTestRunnerEarl(testCase)).execute(options -> {


                            JsonLdOptions expandOptions = new JsonLdOptions(options);

                            expandOptions.setDocumentLoader(
                                                new UriBaseRewriter(
                                                            JsonLdTestCase.TESTS_BASE,
                                                            wireMockServer.baseUrl(),
                                                            SchemeRouter.defaultInstance()));

                            JsonArray r = JsonLd.expand(testCase.input).options(expandOptions).get();

                            return JsonDocument.of(r);
                    });

                    server.stop();
                    wireMockServer.stop();

                } catch (JsonLdError e) {
                    result = false;
                }

                printResult(writer, testCase.uri, result);
            });
    }

    void printResult(PrintWriter writer, String testUri, boolean passed) {

        if (!passed) {
            System.out.println("Failed: " + testUri);
        }

        writer.println();
        writer.println("[ a earl:Assertion;");
        writer.println("  earl:assertedBy <https://github.com/filip26>;");
        writer.println("  earl:subject <https://github.com/filip26/titanium-json-ld>;");
        writer.println("  earl:test <" + testUri + ">;");
        writer.println("  earl:result [");
        writer.println("    a earl:TestResult;");
        writer.println("    earl:outcome " + (passed ? "earl:passed" : "earl:failed") + ";");
        writer.println("    dc:date \"" + DateTimeFormatter.ISO_INSTANT.format(Instant.now().truncatedTo(ChronoUnit.SECONDS)) + "\"^^xsd:dateTime");
        writer.println("  ];");
        writer.println("  earl:mode earl:automatic;");
        writer.println("] .");
    }

    void printHeader(PrintWriter writer) {

        writer.println("@prefix dc: <http://purl.org/dc/terms/> .");
        writer.println("@prefix doap: <http://usefulinc.com/ns/doap#> .");
        writer.println("@prefix foaf: <http://xmlns.com/foaf/0.1/> .");
        writer.println("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .");
        writer.println("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .");
        writer.println("@prefix earl: <http://www.w3.org/ns/earl#> .");
        writer.println();
        writer.println("<> foaf:primaryTopic <https://github.com/filip26/titanium-json-ld>;");
        writer.println("  dc:issued \"" + DateTimeFormatter.ISO_INSTANT.format(Instant.now().truncatedTo(ChronoUnit.SECONDS)) + "\"^^xsd:dateTime;");
        writer.println("  foaf:maker <https://github.com/filip26>.");
        writer.println();
        writer.println("<https://github.com/filip26/titanium-json-ld> a earl:TestSubject,");
        writer.println("    doap:Project,");
        writer.println("    earl:Software;");
        writer.println("  dc:title \"Titanium\" ;");
        writer.println("  dc:creator <https://github.com/filip26>;");
        writer.println("  doap:name \"Titanium\";");
        writer.println("  doap:description \"A JSON-LD 1.1 Processor & API for Java\";");
        writer.println("  doap:developer <https://github.com/filip26>;");
        writer.println("  doap:homepage <https://github.com/filip26/titanium-json-ld>;");
        writer.println("  doap:license <https://github.com/filip26/titanium-json-ld/blob/main/LICENSE>;");
        writer.println("  doap:release [");
        writer.println("    doap:name \"Titanium v" + VERSION + "\";");
        writer.println("    doap:revision \"" + VERSION + "\";");
        writer.println("    doap:created \"" + RELEASE_DATE + "\"^^xsd:date;");
        writer.println("  ] ;");
        writer.println("  doap:programming-language \"Java\".");
        writer.println();
        writer.println("<https://github.com/filip26> a earl:Assertor, foaf:Person;");
        writer.println("  foaf:name \"Filip Kolarik\";");
        writer.println("  foaf:homepage <https://github.com/filip26>.");
    }
}
