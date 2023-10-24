package com.hasmac.jsonld.benchmark;

import com.hasmac.jsonld.JsonLdError;
import com.hasmac.jsonld.api.ToRdfApi;
import com.hasmac.jsonld.document.Document;
import com.hasmac.jsonld.loader.DocumentLoaderOptions;
import com.hasmac.jsonld.loader.FileLoader;
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
@Warmup(iterations = 5)
@BenchmarkMode({Mode.AverageTime})
@Fork(value = 1, jvmArgs = {"-Xmx1024M", "-Xms1024M"})
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class ToRdfLargeFilesBenchmark {

    private Document datagovbeDcat;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        URL fileUrl = getClass().getClassLoader().getResource("benchmark/datagovbe/dcat.jsonld");

        datagovbeDcat = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
    }

    public static void main(String[] args) throws RunnerException {

        // The classe(s) that are included may not get compiled by your IDE.
        // Run `mvn clean verify -DskipTests` before running the benchmarks.

        Options opt = new OptionsBuilder()
                .include(ToRdfLargeFilesBenchmark.class.getName()+".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    public Object datagovbeDcat() throws JsonLdError {
        return new ToRdfApi(datagovbeDcat).get();
    }

}
