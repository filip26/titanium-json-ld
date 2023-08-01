## Original results
```
Benchmark                     Mode  Cnt     Score     Error  Units
LoadingBenchmark.toRdfApiGet  avgt    5  5859.114 ± 522.500  ms/op
```

```
Benchmark                                   Mode  Cnt     Score     Error  Units
BasicProcessingAlgorithmsBenchmark.compact  avgt    5  1896.694 ±  81.070  ms/op
BasicProcessingAlgorithmsBenchmark.expand   avgt    5   673.813 ±   9.570  ms/op
BasicProcessingAlgorithmsBenchmark.flatten  avgt    5  3586.524 ± 124.667  ms/op

```


#### OOMBenchmark.toRdfApiGet
```
Iteration   1: 7466.977 ms/op
Iteration   2: 6891.456 ms/op
Iteration   3: Terminating due to java.lang.OutOfMemoryError: Java heap space
```


## Current results
```
Benchmark                     Mode  Cnt     Score     Error  Units
LoadingBenchmark.toRdfApiGet  avgt    5  1998.111 ± 135.659  ms/op
```

```
Benchmark                                   Mode  Cnt     Score    Error  Units
BasicProcessingAlgorithmsBenchmark.compact  avgt    5  1541.122 ± 33.854  ms/op
BasicProcessingAlgorithmsBenchmark.expand   avgt    5   376.108 ± 17.546  ms/op
BasicProcessingAlgorithmsBenchmark.flatten  avgt    5   816.228 ± 28.191  ms/op
```

#### OOMBenchmark.toRdfApiGet
```
Iteration   1: 2420.124 ms/op
Iteration   2: 1260.448 ms/op
Iteration   3: 1204.927 ms/op
Iteration   4: 1191.012 ms/op
Iteration   5: 1193.424 ms/op
Iteration   6: 1209.333 ms/op
Iteration   7: Terminating due to java.lang.OutOfMemoryError: Java heap space
```
