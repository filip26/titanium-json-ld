# JSON-LD 1.1 Processor

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) specification in Java utilizing [JSONP](https://javaee.github.io/jsonp/) (Java API for JSON Processing).


[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

###  Status

*work in progress*

 Feature | Tests | Pass | Status
 --- | ---: | ---: | ---:
[Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 378 |  338 | 88%
[Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | | | TBD
[Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | | | TBD
[Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | | | TBD
[RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | | | TBD

## Roadmap

Version | Features | 
--- | ---
0.1 | Expansion |
0.2 | Compaction 
0.3 | Flattening
0.4 | RDF serizalization
0.5 | RDF deserialization
0.6 | Framing
0.7 | JsonLdProcessor API
0.8 | Issues, code cleaning and optimization
0.9 | reserved - TBD
1.0 | GA

## Notes
* uses `java.net.URI`

