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

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@State(Scope.Benchmark)
@Warmup(iterations = 5)
@BenchmarkMode({ Mode.AverageTime })
@Fork(value = 1, jvmArgs = { "-Xmx1024M", "-Xms1024M" })
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class LoadingBenchmark {

    Document document;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        URL fileUrl = getClass().getClassLoader().getResource("benchmark/datagovbe-valid.jsonld");

        document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
    }

    public static void main(String[] args) throws URISyntaxException, JsonLdError {
        LoadingBenchmark loadingBenchmark = new LoadingBenchmark();
        for (int i = 0; i<20; i++){
            System.out.println("Iteration " + i);
            loadingBenchmark.setUp();
            loadingBenchmark.toRdfApiGet();
        }
    }

    @Benchmark
    public int toRdfApiGet() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(document).get();

        return rdfDataset.size();
    }

}
