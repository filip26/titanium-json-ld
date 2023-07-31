## Original results
```
Benchmark                     Mode  Cnt     Score     Error  Units
LoadingBenchmark.toRdfApiGet  avgt    5  5859.114 ± 522.500  ms/op
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
LoadingBenchmark.toRdfApiGet  avgt    5  3363.833 ± 448.450  ms/op
```

#### OOMBenchmark.toRdfApiGet
```
Iteration   1: 3271.161 ms/op
Iteration   2: 1867.856 ms/op
Iteration   3: 1798.996 ms/op
Iteration   4: 1805.217 ms/op
Iteration   5: Terminating due to java.lang.OutOfMemoryError: Java heap space
```
