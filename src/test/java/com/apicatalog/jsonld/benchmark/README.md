## Original results
```
Lower is better

Benchmark                     Mode  Cnt     Score     Error  Units
LoadingBenchmark.toRdfApiGet  avgt    5  5859.114 ± 522.500  ms/op
```

```
Lower is better

Benchmark                                   Mode  Cnt     Score     Error  Units
BasicProcessingAlgorithmsBenchmark.compact  avgt    5  1896.694 ±  81.070  ms/op
BasicProcessingAlgorithmsBenchmark.expand   avgt    5   673.813 ±   9.570  ms/op
BasicProcessingAlgorithmsBenchmark.flatten  avgt    5  3586.524 ± 124.667  ms/op
```

```
Lower is better

Benchmark                                          Mode  Cnt  Score   Error  Units
LoadingSmallFilesBenchmark.csiro                   avgt    5  0.227 ± 0.021  ms/op
LoadingSmallFilesBenchmark.difiDataset             avgt    5  3.080 ± 0.075  ms/op
LoadingSmallFilesBenchmark.geonorge                avgt    5  0.034 ± 0.007  ms/op
LoadingSmallFilesBenchmark.schemaExample1          avgt    5  2.646 ± 0.024  ms/op
LoadingSmallFilesBenchmark.schemaExample2          avgt    5  2.902 ± 0.221  ms/op
LoadingSmallFilesBenchmark.schemaExample3          avgt    5  2.650 ± 0.250  ms/op
LoadingSmallFilesBenchmark.schemaExample4          avgt    5  2.596 ± 0.028  ms/op
LoadingSmallFilesBenchmark.schemaExtBib            avgt    5  3.184 ± 0.124  ms/op
LoadingSmallFilesBenchmark.schemaExtHealthLifeSci  avgt    5  7.511 ± 0.108  ms/op
LoadingSmallFilesBenchmark.schemaExtMeta           avgt    5  2.787 ± 0.291  ms/op
```

#### OOMBenchmark.toRdfApiGet
```
More iterations is better

Iteration   1: 7466.977 ms/op
Iteration   2: 6891.456 ms/op
Iteration   3: Terminating due to java.lang.OutOfMemoryError: Java heap space
```

#### JSON-LD Benchmarks - https://github.com/umbreak/jsonld-benchmarks/
```
NB: These benchmarks use ops/s instead of ms/op, so higher is better. 

Benchmark                                          Mode  Cnt     Score    Error  Units
JsonLdImplementationsBenchmark.compactJsonLdJava  thrpt   10   607.079 ± 16.393  ops/s
JsonLdImplementationsBenchmark.expandJsonLdJava   thrpt   10  1158.664 ± 19.302  ops/s
JsonLdImplementationsBenchmark.flattenJsonLdJava  thrpt   10  1557.635 ± 32.629  ops/s
JsonLdImplementationsBenchmark.frameJsonLdJava    thrpt   10   591.227 ±  7.902  ops/s
JsonLdImplementationsBenchmark.fromRdfJsonLdJava  thrpt   10  1529.213 ± 17.981  ops/s
JsonLdImplementationsBenchmark.toRdfJsonLdJava    thrpt   10   440.860 ±  6.484  ops/s

JsonLdImplementationsBenchmark.compactTitanium    thrpt   10   541.109 ± 38.407  ops/s
JsonLdImplementationsBenchmark.expandTitanium     thrpt   10  1014.937 ± 11.538  ops/s
JsonLdImplementationsBenchmark.flattenTitanium    thrpt   10  1354.803 ± 44.767  ops/s
JsonLdImplementationsBenchmark.frameTitanium      thrpt   10   385.913 ±  8.078  ops/s
JsonLdImplementationsBenchmark.fromRdfTitanium    thrpt   10  2169.939 ± 27.233  ops/s
JsonLdImplementationsBenchmark.toRdfTitanium      thrpt   10   375.482 ±  7.964  ops/s
```




## Current results
```
Lower is better

Benchmark                     Mode  Cnt     Score     Error  Units
LoadingBenchmark.toRdfApiGet  avgt    5  1858.771 ± 94.786  ms/op
```

```
Lower is better

Benchmark                                   Mode  Cnt     Score     Error  Units
BasicProcessingAlgorithmsBenchmark.compact  avgt    5  1558.660 ± 127.727  ms/op
BasicProcessingAlgorithmsBenchmark.expand   avgt    5   377.491 ±  12.128  ms/op
BasicProcessingAlgorithmsBenchmark.flatten  avgt    5   811.637 ±  47.212  ms/op
```

```
Lower is better

Benchmark                                          Mode  Cnt  Score    Error  Units
LoadingSmallFilesBenchmark.csiro                   avgt    5  0.110 ±  0.006  ms/op
LoadingSmallFilesBenchmark.difiDataset             avgt    5  1.321 ±  0.040  ms/op
LoadingSmallFilesBenchmark.geonorge                avgt    5  0.013 ±  0.001  ms/op
LoadingSmallFilesBenchmark.schemaExample1          avgt    5  1.236 ±  0.006  ms/op
LoadingSmallFilesBenchmark.schemaExample2          avgt    5  1.249 ±  0.079  ms/op
LoadingSmallFilesBenchmark.schemaExample3          avgt    5  1.306 ±  0.051  ms/op
LoadingSmallFilesBenchmark.schemaExample4          avgt    5  1.286 ±  0.005  ms/op
LoadingSmallFilesBenchmark.schemaExtBib            avgt    5  1.560 ±  0.015  ms/op
LoadingSmallFilesBenchmark.schemaExtHealthLifeSci  avgt    5  3.562 ±  0.018  ms/op
LoadingSmallFilesBenchmark.schemaExtMeta           avgt    5  1.365 ±  0.009  ms/op
```

#### OOMBenchmark.toRdfApiGet
```
More iterations is better

Iteration   1: 2619.442 ms/op
Iteration   2: 1474.245 ms/op
Iteration   3: 1467.797 ms/op
Iteration   4: 1563.119 ms/op
Iteration   5: 1623.688 ms/op
Iteration   6: 1647.819 ms/op
Iteration   7: 1568.830 ms/op
Iteration   8: Terminating due to java.lang.OutOfMemoryError: Java heap space
```

#### JSON-LD Benchmarks - https://github.com/umbreak/jsonld-benchmarks/
```
NB: These benchmarks use ops/s instead of ms/op, so higher is better. 

Benchmark                                        Mode  Cnt     Score    Error  Units
JsonLdImplementationsBenchmark.compactJsonLdJava  thrpt   10   607.079 ± 16.393  ops/s
JsonLdImplementationsBenchmark.expandJsonLdJava   thrpt   10  1158.664 ± 19.302  ops/s
JsonLdImplementationsBenchmark.flattenJsonLdJava  thrpt   10  1557.635 ± 32.629  ops/s
JsonLdImplementationsBenchmark.frameJsonLdJava    thrpt   10   591.227 ±  7.902  ops/s
JsonLdImplementationsBenchmark.fromRdfJsonLdJava  thrpt   10  1529.213 ± 17.981  ops/s
JsonLdImplementationsBenchmark.toRdfJsonLdJava    thrpt   10   440.8a60 ±  6.484  ops/s

Benchmark                                        Mode  Cnt     Score    Error  Units
JsonLdImplementationsBenchmark.compactTitanium  thrpt   10   683.737 ±  9.222  ops/s
JsonLdImplementationsBenchmark.expandTitanium   thrpt   10  1243.785 ± 23.390  ops/s
JsonLdImplementationsBenchmark.flattenTitanium  thrpt   10  1570.057 ± 50.235  ops/s
JsonLdImplementationsBenchmark.frameTitanium    thrpt   10   436.802 ±  8.350  ops/s
JsonLdImplementationsBenchmark.fromRdfTitanium  thrpt   10  2236.124 ± 89.612  ops/s
JsonLdImplementationsBenchmark.toRdfTitanium    thrpt   10   431.800 ±  6.175  ops/s
```
