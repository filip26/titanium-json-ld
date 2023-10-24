package no.hasmac.jsonld.benchmark;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.api.ExpansionApi;
import no.hasmac.jsonld.api.FlatteningApi;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;
import no.hasmac.jsonld.loader.FileLoader;
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
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.TimeUnit;

/**
 * This benchmark is used to compare the performance of the main processing algorithms of JSON-LD.
 */
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@BenchmarkMode({Mode.AverageTime})
@Fork(value = 1, jvmArgs = {"-Xmx2048M", "-Xms2048M"})
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class BasicProcessingAlgorithmsBenchmark {

    private Document datagovbeDcat;
    private Document datagovbeDcatContext;
    private Document datagovbeDcatCompact;
    private Document datagovbeDcatFlatten;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        datagovbeDcat = loadDocument("benchmark/datagovbe/dcat.jsonld");
        datagovbeDcatCompact = loadDocument("benchmark/datagovbe/dcat-compact.jsonld");
        datagovbeDcatFlatten = loadDocument("benchmark/datagovbe/dcat-flatten.jsonld");
        datagovbeDcatContext = loadDocument("benchmark/datagovbe/context/context.jsonld");
    }

    public static void main(String[] args) throws RunnerException {

        // The classe(s) that are included may not get compiled by your IDE.
        // Run `mvn clean verify -DskipTests` before running the benchmarks.

        Options opt = new OptionsBuilder()
                .include(BasicProcessingAlgorithmsBenchmark.class.getName()+".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    public JsonObject compactDatagovbeDcat() throws JsonLdError {
        return JsonLd.compact(datagovbeDcat, datagovbeDcatContext).get();
    }

    @Benchmark
    public JsonObject compactDatagovbeDcatEmptyContext() throws JsonLdError {
        return JsonLd.compact(datagovbeDcat, JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
    }

    @Benchmark
    public JsonArray expandDatagovbeDcatFromCompact() throws JsonLdError {
        return new ExpansionApi(datagovbeDcatCompact).context(datagovbeDcatContext.getJsonContent().get()).get();
    }

    @Benchmark
    public JsonArray expandDatagovbeDcatFromFlatten() throws JsonLdError {
        return JsonLd.expand(datagovbeDcatFlatten).get();
    }

    @Benchmark
    public JsonStructure flattenDatagovbeDcat() throws JsonLdError {
        return JsonLd.flatten(datagovbeDcat).get();
    }

    @Benchmark
    public JsonStructure flattenDatagovbeDcatFromCompact() throws JsonLdError {
        return new FlatteningApi(datagovbeDcatCompact).context(datagovbeDcatContext.getJsonContent().get()).get();
    }

    private Document loadDocument(String name) throws JsonLdError, URISyntaxException {
        URL fileUrl = getClass().getClassLoader().getResource(name);
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        return document;
    }

}
