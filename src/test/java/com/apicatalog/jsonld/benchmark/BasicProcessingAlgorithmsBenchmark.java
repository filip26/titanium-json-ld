package com.apicatalog.jsonld.benchmark;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.FileLoader;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.TimeUnit;

/**
 * This benchmark is used to compare the performance of the main processing algorithms of JSON-LD.
 */
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@BenchmarkMode({Mode.AverageTime})
@Fork(value = 1, jvmArgs = {"-Xmx1024M", "-Xms1024M"})
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class BasicProcessingAlgorithmsBenchmark {

    Document document;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        URL fileUrl = getClass().getClassLoader().getResource("benchmark/datagovbe-valid.jsonld");

        document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
    }

    public static void main(String[] args) throws URISyntaxException, JsonLdError {
        BasicProcessingAlgorithmsBenchmark loadingBenchmark = new BasicProcessingAlgorithmsBenchmark();
        for (int i = 0; i < 20; i++) {
            System.out.println("Iteration " + i);
            loadingBenchmark.setUp();
            loadingBenchmark.flatten();
        }
    }

    @Benchmark
    public JsonObject compact() throws JsonLdError {
        return JsonLd.compact(document, JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
    }

    @Benchmark
    public JsonArray expand() throws JsonLdError {
        return JsonLd.expand(document).get();
    }


    @Benchmark
    public JsonStructure flatten() throws JsonLdError {
        return JsonLd.flatten(document).get();
    }

}
