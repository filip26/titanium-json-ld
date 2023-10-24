package no.hasmac.jsonld.benchmark;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.api.ToRdfApi;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.loader.DocumentLoaderOptions;
import no.hasmac.jsonld.loader.FileLoader;
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
public class ToRdfSmallFilesBenchmark {

    private Document csiro;
    private Document difiDataset;
    private Document geonorge;
    private Document schemaExample1;
    private Document schemaExample2;
    private Document schemaExample3;
    private Document schemaExample4;
    private Document schemaExtBib;
    private Document schemaExtHealthLifeSci;
    private Document schemaExtMeta;
    private JsonStructure schemaContext;

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


    public static void main(String[] args) throws RunnerException {

        // The classe(s) that are included may not get compiled by your IDE.
        // Run `mvn clean verify -DskipTests` before running the benchmarks.

        Options opt = new OptionsBuilder()
                .include(ToRdfSmallFilesBenchmark.class.getName()+".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    public Object csiro() throws JsonLdError {
        return new ToRdfApi(csiro).get();
    }

    @Benchmark
    public Object difiDataset() throws JsonLdError {
        return new ToRdfApi(difiDataset).get();
    }

    @Benchmark
    public Object geonorge() throws JsonLdError {
        return new ToRdfApi(geonorge).get();
    }

    @Benchmark
    public Object schemaExample1() throws JsonLdError {
        return new ToRdfApi(schemaExample1).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExample2() throws JsonLdError {
        return new ToRdfApi(schemaExample2).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExample3() throws JsonLdError {
        return new ToRdfApi(schemaExample3).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExample4() throws JsonLdError {
        return new ToRdfApi(schemaExample4).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExtBib() throws JsonLdError {
        return new ToRdfApi(schemaExtBib).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExtHealthLifeSci() throws JsonLdError {
        return new ToRdfApi(schemaExtHealthLifeSci).context(schemaContext).get();
    }

    @Benchmark
    public Object schemaExtMeta() throws JsonLdError {
        return new ToRdfApi(schemaExtMeta).context(schemaContext).get();
    }

    private Document loadDocument(String name) throws JsonLdError, URISyntaxException {
        URL fileUrl = getClass().getClassLoader().getResource(name);
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        return document;
    }

}
