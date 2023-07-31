package com.apicatalog.jsonld.benchmark;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.api.ToRdfApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.FileLoader;
import com.apicatalog.rdf.RdfDataset;
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

    Document document;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        document = null;
        URL fileUrl = getClass().getClassLoader().getResource("benchmark/datagovbe-valid.jsonld");
        document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
    }

    @Benchmark
    public int toRdfApiGet() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(document).get();
        return rdfDataset.size();
    }

}
