package com.apicatalog.jsonld.benchmark;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.api.ToRdfApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.jsonld.loader.FileLoader;
import com.apicatalog.rdf.RdfDataset;
import jakarta.json.JsonStructure;
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
@Warmup(iterations = 5)
@BenchmarkMode({Mode.AverageTime})
@Fork(value = 1, jvmArgs = {"-Xmx1024M", "-Xms1024M"})
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class LoadingSmallFilesBenchmark {

    Document csiro;
    Document difiDataset;
    Document geonorge;
    Document schemaExample1;
    Document schemaExample2;
    Document schemaExample3;
    Document schemaExample4;
    Document schemaExtBib;
    Document schemaExtHealthLifeSci;
    Document schemaExtMeta;
    JsonStructure schemaContext;

    @Setup(Level.Invocation)
    public void setUp() throws URISyntaxException, JsonLdError {
        csiro = loadDocument("benchmark/digdir/dxwg/csiro-stratchart.jsonld");
        difiDataset = loadDocument("benchmark/digdir/fdk/difi-dataset-2017-10-19.jsonld");
        geonorge = loadDocument("benchmark/digdir/fdk/location_geonorge_kommune.jsonld");
        schemaExample1 = loadDocument("benchmark/schemaorg/example1.jsonld");
        schemaExample2 = loadDocument("benchmark/schemaorg/example2.jsonld");
        schemaExample3 = loadDocument("benchmark/schemaorg/example3.jsonld");
        schemaExample4 = loadDocument("benchmark/schemaorg/example4.jsonld");
        schemaExtBib = loadDocument("benchmark/schemaorg/ext-bib.jsonld");
        schemaExtHealthLifeSci = loadDocument("benchmark/schemaorg/ext-health-lifesci.jsonld");
        schemaExtMeta = loadDocument("benchmark/schemaorg/ext-meta.jsonld");
        schemaContext = loadDocument("benchmark/schemaorg/schemaorgcontext.jsonld").getJsonContent().get();
    }


    public static void main(String[] args) throws URISyntaxException, JsonLdError {
        LoadingSmallFilesBenchmark loadingBenchmark = new LoadingSmallFilesBenchmark();
        for (int i = 0; i < 5000; i++) {
            System.out.println("Iteration " + i);
            loadingBenchmark.setUp();
            loadingBenchmark.schemaExample4();
        }
    }

    @Benchmark
    public int csiro() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(csiro).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int difiDataset() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(difiDataset).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int geonorge() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(geonorge).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExample1() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExample1).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExample2() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExample2).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExample3() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExample3).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExample4() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExample4).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExtBib() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExtBib).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExtHealthLifeSci() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExtHealthLifeSci).context(schemaContext).get();
        return rdfDataset.size();
    }

    @Benchmark
    public int schemaExtMeta() throws JsonLdError {
        RdfDataset rdfDataset = new ToRdfApi(schemaExtMeta).context(schemaContext).get();
        return rdfDataset.size();
    }

    private Document loadDocument(String name) throws JsonLdError, URISyntaxException {
        URL fileUrl = getClass().getClassLoader().getResource(name);
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        return document;
    }

}
