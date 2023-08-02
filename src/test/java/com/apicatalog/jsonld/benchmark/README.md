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
LoadingBenchmark.toRdfApiGet  avgt    5  1998.111 ± 135.659  ms/op
```

```
Lower is better

Benchmark                                   Mode  Cnt     Score     Error  Units
BasicProcessingAlgorithmsBenchmark.compact  avgt    5  1582.788 ± 134.656  ms/op
BasicProcessingAlgorithmsBenchmark.expand   avgt    5   364.495 ±  11.532  ms/op
BasicProcessingAlgorithmsBenchmark.flatten  avgt    5   809.893 ±   6.818  ms/op
```

#### OOMBenchmark.toRdfApiGet
```
More iterations is better

Iteration   1: 2420.124 ms/op
Iteration   2: 1260.448 ms/op
Iteration   3: 1204.927 ms/op
Iteration   4: 1191.012 ms/op
Iteration   5: 1193.424 ms/op
Iteration   6: 1209.333 ms/op
Iteration   7: Terminating due to java.lang.OutOfMemoryError: Java heap space
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
