package com.apicatalog.jsonld.earl;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.suite.JsonLdManifestLoader;
import com.apicatalog.jsonld.suite.JsonLdTestRunnerEarl;

public class EarlGenerator {

    public static void main(String[] args) throws IOException {
        (new EarlGenerator()).generate(Paths.get("java-jsonp-ld-earl.ttl"));
    }

    public void generate(final Path path) throws IOException {
        
        try (PrintWriter writer = new PrintWriter(path.toFile())) {
            
            printHeader(writer);
            testExpand(writer);
            testCompact(writer);
        };
    }
    
    public void testExpand(PrintWriter writer) throws IOException {

        JsonLdManifestLoader
            .load("expand-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                                (new JsonLdTestRunnerEarl(testCase)).execute(options ->
                                
                                    JsonLd.expand(testCase.input).options(options).get()
                                )
                         )
                    );
    }

    public void testCompact(final PrintWriter writer) throws IOException {

        JsonLdManifestLoader
            .load("compact-manifest.jsonld")
            .stream()
            .forEach(testCase ->                
                        printResult(writer, testCase.uri,           
                             (new JsonLdTestRunnerEarl(testCase)).execute(options -> {
                            
                                    //pre-load context
                                    RemoteDocument jsonContext = options.getDocumentLoader().loadDocument(testCase.context, new LoadDocumentOptions());
                                                                    
                                    return JsonLd.compact(
                                                        testCase.input, 
                                                        jsonContext.getDocument().asJsonStructure()
                                                        )
                                                    .options(options)
                                                    .get();
                                 })
                         )
                    );
    }
    
    void printResult(PrintWriter writer, String testUri, boolean passed) {
        writer.println();
        writer.println("[ a earl:Assertion;");
        writer.println("  earl:assertedBy <https://github.com/filip26>;");
        writer.println("  earl:subject <https://github.com/filip26/jsonp-ld>;");
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
        writer.println("<> foaf:primaryTopic <https://github.com/filip26/jsonp-ld>;");
        writer.println("  dc:issued \"" + DateTimeFormatter.ISO_INSTANT.format(Instant.now().truncatedTo(ChronoUnit.SECONDS)) + "\"^^xsd:dateTime;");
        writer.println("  foaf:maker <https://github.com/filip26>.");
        writer.println();
        writer.println("<https://github.com/filip26/jsonp-ld> a earl:TestSubject,");
        writer.println("    doap:Project,");
        writer.println("    earl:Software;");
        writer.println("  dc:title \"JSONP-LD\" ;");
        writer.println("  dc:creator <https://github.com/filip26>;");
        writer.println("  doap:name \"JSONP-LD\";");
        writer.println("  doap:description \"A JSON-LD 1.1 Processor & API for Java\";");
        writer.println("  doap:developer <https://github.com/filip26>;");
        writer.println("  doap:homepage <https://github.com/filip26/jsonp-ld>;");
        writer.println("  doap:license <https://github.com/filip26/jsonp-ld/blob/master/LICENSE>;");
        writer.println("  doap:release [");
        writer.println("    doap:name \"JSONP-LD 0.2.2\";");
        writer.println("    doap:revision \"0.2.2\";");
        writer.println("    doap:created \"2020-05-30\"^^xsd:date;");
        writer.println("  ] ;");
        writer.println("  doap:programming-language \"Java\".");
        writer.println();
        writer.println("<https://github.com/filip26> a earl:Assertor, foaf:Person;");
        writer.println("  foaf:name \"Filip Kolarik\";");
        writer.println("  foaf:homepage <https://github.com/filip26>.");
    }
    
}
