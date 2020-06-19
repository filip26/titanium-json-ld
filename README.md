# JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java utilizing [JSONP](https://javaee.github.io/jsonp/) (JSR 374 - Java API for JSON Processing).

![Java CI with Maven](https://github.com/filip26/titanium-json-ld/workflows/Java%20CI%20with%20Maven/badge.svg)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/c530c6b43b0243c08ce81521c5b4cf6a)](https://www.codacy.com/manual/filip26/titanium-json-ld?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=filip26/titanium-json-ld&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/c530c6b43b0243c08ce81521c5b4cf6a)](https://www.codacy.com/manual/filip26/titanium-json-ld?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=filip26/titanium-json-ld&amp;utm_campaign=Badge_Coverage)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

###  Status

 Feature | Tests | Pass | Status | Notes
 --- | ---: | ---: | ---: | ---
[Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 369 |  369 | 100% | 
[Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 239 | 239 | 100% |
[Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | 55 | 55 | 100% |
[JSON-LD to RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 449 | 447 | 99.5% | <ul><li>[te075 - @vocab as blank node identifier](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#te075)</li><li>[tli12 - List with bad @base](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#tli12)</li></ul>
[RDF to JSON-LD](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 51 | 51  | 100% | 
[Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | 89 | 88 | 98.8% | <ul><li>[t0059 - @embed: @last](https://w3c.github.io/json-ld-framing/tests/frame-manifest#t0059)</li></ul>
[Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval) | 18 | 17 | 94.4% | <ul><li>[t0013 - HTML document](https://w3c.github.io/json-ld-api/tests/remote-doc-manifest#t0013)</li></ul>

See [EARL results from the JSON-LD 1.1 Test Suite](https://w3c.github.io/json-ld-api/reports/#subj_Titanium_JSON_LD_Java) for more details.

## Installation

TBD

## Roadmap

- [x] 0.1 - ~~[Expansion Algorithms](https://www.w3.org/TR/json-ld11-api/#expansion-algorithms)~~
- [x] 0.2 - ~~[Compaction Algorithms](https://www.w3.org/TR/json-ld11-api/#compaction-algorithms)~~
- [x] 0.3 - ~~[Flattening Algorithms](https://www.w3.org/TR/json-ld11-api/#flattening-algorithms)~~
- [x] 0.4 - ~~[JSON-LD to RDF](https://www.w3.org/TR/json-ld11-api/#deserialize-json-ld-to-rdf-algorithm)~~
- [x] 0.5 - ~~[RDF to JSON-LD](https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm)~~
- [x] 0.6 - ~~[Framing](https://www.w3.org/TR/json-ld11-framing/)~~
- [x] 0.7 - ~~[Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval)~~
- [ ] 0.8 - [JSON-LD API](https://www.w3.org/TR/json-ld11-api/#the-application-programming-interface)
- [ ] 1.0 - Documented, A+ code

## Examples

Titanium implements the [JsonLdProcessor](https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface) interface and custom interface based on builder design pattern.

#### Builder API 

```javascript

// Expansion
JsonLd.expand("https://w3c.github.io/json-ld-api/tests/expand/0001-in.jsonld")
      .ordered()
      .get();

JsonLd.expand("https://example.com/document.json")
      .context("https://example.com/context.jsonld")  // external context
      .get();

// Compaction
JsonLd.compact("https://example.com/expanded.jsonld", "https://example.com/context.jsonld").get();

// Flattening
JsonLd.flatten("https://example.com/document.jsonld").get();

// JSON-LD to RDF
JsonLd.toRdf("https://example.com/document.jsonld").get();

// RDF to JSON-LD
RdfDataset dataset = Rdf.createReader(reader, RdfFormat.N_QUADS).readDataset();

JsonLd.fromRdf(dataset).options(options).get();

// Framing
JsonLd.frame("https://example.com/document.jsonld", "https://example.com/frame.jsonld").get();

```
## Contributing

Your contribution is welcome. 

### Building

Fork and clone the project repository:

```bash
> git clone git@github.com:filip26/titanium-json-ld.git
```

#### Install test packages

Download [json-ld-test-suite](https://github.com/filip26/json-ld-api/packages/248430) and [json-ld-framing-test-suite](https://github.com/filip26/json-ld-api/packages/261048). 

```bash
> mvn install:install-file -Dfile=<path-to-json-ld-test-suite> -DgroupId=com.github.w3c \
-DartifactId=json-ld-test-suite -Dversion=20200518 -Dpackaging=jar

> mvn install:install-file -Dfile=<path-to-json-ld-framing-test-suite> -DgroupId=com.github.w3c \
-DartifactId=json-ld-framing-test-suite -Dversion=20200609 -Dpackaging=jar

```
#### Compile sources

```bash
> mvn package
```
Each pull request must pass tests.


