package no.hasmac.jsonld.benchmark;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.api.ToRdfApi;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;
import no.hasmac.jsonld.loader.FileLoader;
import no.hasmac.rdf.RdfDataset;
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

@State(Scope.Benchmark)
@Warmup(iterations = 0)
@BenchmarkMode({Mode.AverageTime})
@Fork(value = 1, jvmArgs = {"-Xms16G", "-Xmx16G", "-XX:+UnlockExperimentalVMOptions", "-XX:+UseEpsilonGC",
        "-XX:+AlwaysPreTouch"})
@Measurement(iterations = 99999999, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
/*
 * This benchmark is used to test how many iterations we can run before running out of memory.
 */
public class OOMBenchmark {

    private Document datagovbeDcat;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        datagovbeDcat = null;
        URL fileUrl = getClass().getClassLoader().getResource("benchmark/datagovbe/dcat.jsonld");
        datagovbeDcat = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
    }

    public static void main(String[] args) throws RunnerException {

        // The classe(s) that are included may not get compiled by your IDE.
        // Run `mvn clean verify -DskipTests` before running the benchmarks.

        Options opt = new OptionsBuilder()
                .include(OOMBenchmark.class.getName()+".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    public int datagovbeDcatToRdf() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(datagovbeDcat).get();
        return rdfDataset.size();
    }

}
