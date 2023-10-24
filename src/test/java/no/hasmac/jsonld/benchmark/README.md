```
Benchmark                                                            Mode  Cnt     Score      Error  Units
BasicProcessingAlgorithmsBenchmark.compactDatagovbeDcat              avgt    5  2922.482 ±  152.198  ms/op
BasicProcessingAlgorithmsBenchmark.compactDatagovbeDcatEmptyContext  avgt    5  1693.791 ±  197.799  ms/op
BasicProcessingAlgorithmsBenchmark.expandDatagovbeDcatFromCompact    avgt    5   603.828 ±   51.577  ms/op
BasicProcessingAlgorithmsBenchmark.expandDatagovbeDcatFromFlatten    avgt    5   608.729 ±   51.343  ms/op
BasicProcessingAlgorithmsBenchmark.flattenDatagovbeDcat              avgt    5  1073.333 ±  155.300  ms/op
BasicProcessingAlgorithmsBenchmark.flattenDatagovbeDcatFromCompact   avgt    5  9164.437 ± 1339.912  ms/op
ToRdfLargeFilesBenchmark.datagovbeDcat                               avgt    5  2036.179 ±  108.510  ms/op
ToRdfSmallFilesBenchmark.csiro                                       avgt    5     0.111 ±    0.002  ms/op
ToRdfSmallFilesBenchmark.difiDataset                                 avgt    5     1.332 ±    0.018  ms/op
ToRdfSmallFilesBenchmark.geonorge                                    avgt    5     0.012 ±    0.001  ms/op
ToRdfSmallFilesBenchmark.schemaExample1                              avgt    5     1.256 ±    0.003  ms/op
ToRdfSmallFilesBenchmark.schemaExample2                              avgt    5     1.274 ±    0.022  ms/op
ToRdfSmallFilesBenchmark.schemaExample3                              avgt    5     1.299 ±    0.002  ms/op
ToRdfSmallFilesBenchmark.schemaExample4                              avgt    5     1.285 ±    0.003  ms/op
ToRdfSmallFilesBenchmark.schemaExtBib                                avgt    5     1.517 ±    0.019  ms/op
ToRdfSmallFilesBenchmark.schemaExtHealthLifeSci                      avgt    5     3.579 ±    0.019  ms/op
ToRdfSmallFilesBenchmark.schemaExtMeta                               avgt    5     1.275 ±    0.011  ms/op
```

```
# Benchmark: com.apicatalog.jsonld.benchmark.OOMBenchmark.datagovbeDcatToRdf

# Run progress: 0.59% complete, ETA 1 days, 07:50:34
# Fork: 1 of 1
Iteration   1: 2491.034 ms/op
Iteration   2: 1432.587 ms/op
Iteration   3: 1408.033 ms/op
Iteration   4: 1605.812 ms/op
Iteration   5: 1638.855 ms/op
Iteration   6: 1603.059 ms/op
Iteration   7: 1647.721 ms/op
Iteration   8: Terminating due to java.lang.OutOfMemoryError: Java heap space
```
