# JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java utilizing [JSONP](https://javaee.github.io/jsonp/) (JSR 374 - Java API for JSON Processing).

![Build](https://github.com/filip26/titanium-json-ld/workflows/Java%20CI%20with%20Maven/badge.svg)
[![Language grade: Java](https://img.shields.io/lgtm/grade/java/g/filip26/titanium-json-ld.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/filip26/titanium-json-ld/context:java)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/c530c6b43b0243c08ce81521c5b4cf6a)](https://www.codacy.com/manual/filip26/titanium-json-ld?utm_source=github.com&utm_medium=referral&utm_content=filip26/titanium-json-ld&utm_campaign=Badge_Coverage)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/titanium-json-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.apicatalog%22%20AND%20a:%22titanium-json-ld%22)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Goals
- conformance to the specification
- secure, stable, fast, A+ code
- minimal external dependencies
  - only `javax.json:javax.json-api` is required now
- simple to use

## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

### Status

 | Feature | Tests | Pass | Status | Notes |
 | --- | ---: | ---: | ---: | --- |
| [Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 369 |  369 | 100% | |
| [Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 239 | 239 | 100% | |
| [Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | 55 | 55 | 100% | |
| [JSON-LD to RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 449 | 447 | 99.5% | <ul><li>[te075 - @vocab as blank node identifier](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#te075)</li><li>[tli12 - List with bad @base](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#tli12)</li></ul> |
| [RDF to JSON-LD](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 51 | 51  | 100% | |
| [Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | 89 | 88 | 98.8% | <ul><li>[t0059 - @embed: @last](https://w3c.github.io/json-ld-framing/tests/frame-manifest#t0059)</li></ul> |
| [Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval) | 18 | 17 | 94.4% | <ul><li>[t0013 - HTML document](https://w3c.github.io/json-ld-api/tests/remote-doc-manifest#t0013)</li></ul> |

See [EARL results from the JSON-LD 1.1 Test Suite](https://w3c.github.io/json-ld-api/reports/#subj_Titanium_JSON_LD_Java) for more details.

## Installation

**Titanium JSON-LD**

Maven

```xml
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>titanium-json-ld</artifactId>
    <version>0.8</version>
</dependency>

```

Gradle

```gradle
compile group: 'com.apicatalog', name: 'titanium-json-ld', version: '0.8'
```

**JSONP Provider**

Add JSONP provider, if it is not on the classpath already. 

Maven

```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>javax.json</artifactId>
    <version>1.1.4</version>
</dependency>

```

Gradle

```gradle
compile group: 'org.glassfish', name: 'javax.json', version: '1.1.4'

```
## Documentation

[Javadoc](https://filip26.github.io/titanium-json-ld/apidocs/)

TBD

## Examples

Titanium provides high-level [JsonLd](https://filip26.github.io/titanium-json-ld/apidocs/com/apicatalog/jsonld/JsonLd.html) API to interact with the processor.

```javascript

// Expansion
JsonLd.expand("https://w3c.github.io/json-ld-api/tests/expand/0001-in.jsonld")
      .ordered()
      .get();

JsonLd.expand("file:/home/filip/document.json")    // HTTP(S) and File schemes supported
      .context("file:/home/filip/context.jsonld")  // external context
      .get();

// Compaction
JsonLd.compact("https://example/expanded.jsonld", "https://example/context.jsonld")
      .compactToRelative(false)
      .get();

// Flattening
JsonLd.flatten("https://example/document.jsonld").get();

// JSON-LD to RDF
JsonLd.toRdf("https://example/document.jsonld").get();

// RDF to JSON-LD
JsonLd.fromRdf("https://example/document.nq").options(options).get();

// Framing
JsonLd.frame("https://example/document.jsonld", "https://example/frame.jsonld").get();

```

```javascript
// Local document
Document document = Document.of(InputStream) or Document.of(Reader) ...

JsonLd.expand(document).get();

JsonLd.compact(document, contextDocument).get();
...
```

## Contributing

Your contribution is welcome. 

### Roadmap

- [x] 0.1 ~~[Expansion Algorithms](https://www.w3.org/TR/json-ld11-api/#expansion-algorithms)~~
- [x] 0.2 ~~[Compaction Algorithms](https://www.w3.org/TR/json-ld11-api/#compaction-algorithms)~~
- [x] 0.3 ~~[Flattening Algorithms](https://www.w3.org/TR/json-ld11-api/#flattening-algorithms)~~
- [x] 0.4 ~~[JSON-LD to RDF](https://www.w3.org/TR/json-ld11-api/#deserialize-json-ld-to-rdf-algorithm)~~
- [x] 0.5 ~~[RDF to JSON-LD](https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm)~~
- [x] 0.6 ~~[Framing](https://www.w3.org/TR/json-ld11-framing/)~~
- [x] 0.7 ~~[Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval)~~
- [x] 0.8 ~~JsonLd API~~
- [ ] 0.9 [HTML](https://www.w3.org/TR/json-ld11-api/#html-content-algorithms)
- [ ] 1.0 Documented, A+ code

### Building

Fork and clone the project repository:

```bash
> git clone git@github.com:filip26/titanium-json-ld.git
```

Compile sources:

```bash
> cd titanium-json-ld
> mvn package
```
