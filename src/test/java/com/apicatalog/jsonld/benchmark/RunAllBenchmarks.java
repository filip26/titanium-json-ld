package com.apicatalog.jsonld.benchmark;

import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public class RunAllBenchmarks {

    public static void main(String[] args) throws RunnerException {

        // The classe(s) that are included may not get compiled by your IDE.
        // Run `mvn clean verify -DskipTests` before running the benchmarks.

        Options opt = new OptionsBuilder()
                .include("com.apicatalog.jsonld.benchmark.*.*")
                .build();

        new Runner(opt).run();
    }
}
