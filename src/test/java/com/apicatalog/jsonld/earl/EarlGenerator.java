package com.apicatalog.jsonld.earl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Objects;

import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.RemoteTest;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdMockServer;
import com.apicatalog.jsonld.suite.JsonLdTestCase;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerEarl;
import com.apicatalog.jsonld.suite.loader.UriBaseRewriter;
import com.apicatalog.jsonld.suite.loader.ZipResourceLoader;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.NQuadsReaderException;
import com.github.tomakehurst.wiremock.WireMockServer;

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
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "expand-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                                (new JsonLdTestRunnerEarl(testCase)).execute(options ->
                                
                                    JsonLd.expand(testCase.input).options(options).get()
                                )
                         )
                    );
    }

    public void testCompact(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "compact-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                             (new JsonLdTestRunnerEarl(testCase)).execute(options -> {
                            
                                     //pre-load context
                                    if (testCase.context == null) {
                                        throw new IllegalStateException();
                                    }
                                    
                                    Document contextDocument = options.getDocumentLoader().loadDocument(testCase.context, new LoadDocumentOptions());
                                     
                                    if (contextDocument == null) {
                                        throw new IllegalStateException();
                                    }
                                
                                    JsonStructure jsonContext  = contextDocument.getJsonContent().orElseThrow(IllegalStateException::new);

                                    return JsonLd.compact(
                                                        testCase.input, 
                                                        jsonContext
                                                        )
                                                    .options(options)
                                                    .get();
                                 })
                         )
                    );
    }

    public void testFlatten(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "flatten-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                             (new JsonLdTestRunnerEarl(testCase)).execute(options -> {
                            
                                 JsonStructure jsonContext = null;
                                 
                                 //pre-load context
                                 if (testCase.context != null) {
                                     Document contextDocument = options.getDocumentLoader().loadDocument(testCase.context, new LoadDocumentOptions());
                                     
                                     if (contextDocument == null) {
                                         throw new IllegalStateException();
                                     }

                                     jsonContext = contextDocument.getJsonContent().orElseThrow(IllegalStateException::new);
                                 }
                                                 
                                 return JsonLd
                                             .flatten(testCase.input) 
                                             .context(jsonContext)
                                             .options(options)
                                             .get();
                                 })
                         )
                    );
    }

    public void testToRdf(final PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "toRdf-manifest.jsonld")
            .stream()
            .forEach(testCase -> printResult(writer, testCase.uri, testToRdf(testCase)));
    }

    public void testFromRdf(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "fromRdf-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                    printResult(writer, testCase.uri,           
                            (new JsonLdTestRunnerEarl(testCase)).execute(options -> {

                                byte[] inputDocument = (new ZipResourceLoader()).fetchBytes(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE + testCase.input.toString().substring("https://w3c.github.io/json-ld-api/tests/".length())));

                                if (inputDocument == null)  {
                                    throw new IllegalStateException();
                                }

                                try {
                                    RdfDataset input = Rdf.createReader(new ByteArrayInputStream(inputDocument), RdfFormat.N_QUADS).readDataset();
                                    
                                    return JsonLd.fromRdf(input).options(options).get();
                                    
                                } catch (NQuadsReaderException | IOException | UnsupportedFormatException e) {
                                    return null;
                                }
                            })
                     )
                );
    }

    public void testFrame(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_FRAMING_BASE, "frame-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                                (new JsonLdTestRunnerEarl(testCase)).execute(options ->
                                
                                    JsonLd.frame(testCase.input, testCase.frame).options(options).get()
                                )
                         )
                    );
    }

    
    public void testRemote(PrintWriter writer) throws JsonLdError {

        JsonLdManifestLoader
            .load(JsonLdManifestLoader.JSON_LD_API_BASE, "remote-doc-manifest.jsonld")
            .stream()
            .forEach(testCase -> {
                
                boolean result = false;
                
                try {
                    WireMockServer wireMockServer = new WireMockServer();
                    wireMockServer.start();

                    JsonLdMockServer server = new JsonLdMockServer(testCase, RemoteTest.TESTS_BASE);
                    server.start();
                    
                    
                    result = (new JsonLdTestRunnerEarl(testCase)).execute(options -> {
    
                            
                            JsonLdOptions expandOptions = new JsonLdOptions(options);
                            
                            expandOptions.setDocumentLoader(
                                                new UriBaseRewriter(
                                                            RemoteTest.TESTS_BASE, 
                                                            wireMockServer.baseUrl(), 
                                                            new HttpLoader()));
                            
                            JsonValue r = JsonLd.expand(testCase.input).options(expandOptions).get();
                            
                            return r;
                    });
                    
                    server.stop();
                    wireMockServer.stop();
                    
                } catch (Throwable e) {
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
        writer.println("  doap:license <https://github.com/filip26/titanium-json-ld/blob/master/LICENSE>;");
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

    private final boolean testToRdf(JsonLdTestCase testCase) {
        
        JsonLdOptions options = testCase.getOptions();
        
        RdfDataset result = null;

        try {
  
            result = JsonLd.toRdf(testCase.input).options(options).get();
            
            if (result == null) {
                return false;
            }
        
        } catch (JsonLdError e) {
            return Objects.equals(testCase.expectErrorCode, e.getCode());
        }

        if (testCase.expectErrorCode != null) {
            return false;
        }
        
        // A PositiveSyntaxTest succeeds when no error is found when processing.
        if (testCase.expect == null && testCase.type.contains("jld:PositiveSyntaxTest")) {
            return true;
        }
        
        if (testCase.expect == null) {
            return false;
        }
        
        try {

            byte[] expectedDocument = (new ZipResourceLoader()).fetchBytes(URI.create(JsonLdManifestLoader.JSON_LD_API_BASE + testCase.expect.toString().substring("https://w3c.github.io/json-ld-api/tests/".length())));

            if (expectedDocument == null) {
                throw new IllegalStateException();
            }

            RdfDataset expected = Rdf.createReader(new ByteArrayInputStream(expectedDocument), RdfFormat.N_QUADS).readDataset();

            if (expected == null) {
                return false;
            }

            return RdfComparison.equals(expected, result);

        } catch (NQuadsReaderException | IOException | UnsupportedFormatException | JsonLdError e) {

        }
        return false;
    }
    
}
