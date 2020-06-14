# JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java utilizing [JSONP](https://javaee.github.io/jsonp/) (Java API for JSON Processing).

![Java CI with Maven](https://github.com/filip26/titanium-json-ld/workflows/Java%20CI%20with%20Maven/badge.svg)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

###  Status

*work in progress*

 Feature | Tests | Pass | Status | Notes
 --- | ---: | ---: | ---: | ---
[Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 369 |  369 | 100% | 
[Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 239 | 239 | 100% |
[Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | 55 | 55 | 100% |
[JSON-LD to RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 449 | 447 | 99.5% |  won't fix: <ul><li>[te075](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#te075) - blank node as a predicate</li><li>[tli12](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#tli12) - an invalid IRI as @base</li></ul>
[RDF to JSON-LD](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 51 | 51  | 100% | 
[Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | 89 | 73 | 82% |

See [EARL results from the JSON-LD 1.1 Test Suite](https://w3c.github.io/json-ld-api/reports/#subj_Titanium_JSON_LD_Java) for more details.

## Roadmap

- [x] 0.1 - ~~[Expansion Algorithms](https://www.w3.org/TR/json-ld11-api/#expansion-algorithms)~~
- [x] 0.2 - ~~[Compaction Algorithms](https://www.w3.org/TR/json-ld11-api/#compaction-algorithms)~~
- [x] 0.3 - ~~[Flattening Algorithms](https://www.w3.org/TR/json-ld11-api/#flattening-algorithms)~~
- [x] 0.4 - ~~[JSON-LD to RDF](https://www.w3.org/TR/json-ld11-api/#deserialize-json-ld-to-rdf-algorithm)~~
- [x] 0.5 - ~~[RDF to JSON-LD](https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm)~~
- [ ] 0.6 - [Framing](https://www.w3.org/TR/json-ld11-framing/)
- [ ] 0.7 - Document & Context loaders
- [ ] 0.8 - [JSON-LD API](https://www.w3.org/TR/json-ld11-api/#the-application-programming-interface)
- [ ] 0.9 - CLI
- [ ] 1.0 - Documented, A+ code

## Examples

Titanium JSON-LD implements the [JsonLdProcessor](https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface) interface and custom interface based on the builder design pattern.

#### JsonLd Builder API 

```javascript
JsonLd.expand("https://w3c.github.io/json-ld-api/tests/expand/0001-in.jsonld")
      .ordered()
      .get();

JsonLd.expand("https://example.com/document.json")
      .context("https://example.com/context.jsonld")  // external context
      .get();

JsonLd.compact("https://example.com/expanded.jsonld", "https://example.com/context.jsonld").get();

JsonLd.flatten("https://example.com/document.jsonld").get();

JsonLd.toRdf("https://example.com/document.jsonld").get();

RdfDataset dataset = Rdf.createReader(reader, RdfFormat.N_QUADS).readDataset();

JsonLd.fromRdf(dataset).options(options).get();


```


