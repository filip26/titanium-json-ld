# Titanium JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java utilizing [Jakarta JSON Processing](https://github.com/eclipse-ee4j/jsonp).

The goals of Titanium are:
- conformance to the specification
- secure, stable, fast, A+ code
- minimal external dependencies
  - only `jakarta.json-api` is required
- simple to use

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/35f3feed56974e7dbf905c4d52045049)](https://app.codacy.com/gh/filip26/titanium-json-ld?utm_source=github.com&utm_medium=referral&utm_content=filip26/titanium-json-ld&utm_campaign=Badge_Grade_Settings)
[![Java 11 CI](https://github.com/filip26/titanium-json-ld/actions/workflows/java11-build.yml/badge.svg)](https://github.com/filip26/titanium-json-ld/actions/workflows/java11-build.yml)
[![Android (Java 8) CI](https://github.com/filip26/titanium-json-ld/actions/workflows/java8-build.yml/badge.svg)](https://github.com/filip26/titanium-json-ld/actions/workflows/java8-build.yml)
[![CodeQL](https://github.com/filip26/titanium-json-ld/actions/workflows/codeql.yml/badge.svg)](https://github.com/filip26/titanium-json-ld/actions/workflows/codeql.yml)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=filip26_titanium-json-ld&metric=sqale_rating)](https://sonarcloud.io/dashboard?id=filip26_titanium-json-ld)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/c530c6b43b0243c08ce81521c5b4cf6a)](https://www.codacy.com/manual/filip26/titanium-json-ld?utm_source=github.com&utm_medium=referral&utm_content=filip26/titanium-json-ld&utm_campaign=Badge_Coverage)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/titanium-json-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.apicatalog%22%20AND%20a:%22titanium-json-ld%22)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Table of Contents  
- [Conformance](#conformance)  
- [CLI](#cli)
- [Extensions](#extensions)  
- [Usage](#usage)
  * [Installation](#installation)
  * [Documentation](#documentation)
  * [Examples](#examples)
- [Contributing](#contributing)
- [Resources](#resources)
- [Commercial Support](#commercial-support)

## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

### Status

 | Feature | Tests | Pass | Status | Notes |
 | --- | ---: | ---: | ---: | --- |
| [Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 371 |  371 | 100% | |
| [Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 242 | 242 | 100% | |
| [Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | 55 | 55 | 100% | |
| [JSON-LD to RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 451 | 449 | 99.5% | <ul><li>[te075 - @vocab as blank node identifier](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#te075)</li><li>[tli12 - List with bad @base](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#tli12)</li></ul> |
| [RDF to JSON-LD](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 51 | 51  | 100% | |
| [Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | 89 | 88 | 98.8% | <ul><li>[t0059 - @embed: @last](https://w3c.github.io/json-ld-framing/tests/frame-manifest#t0059)</li></ul> |
| [Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval) | 18 | 17 | 94.4% | <ul><li>[t0013 - HTML document](https://w3c.github.io/json-ld-api/tests/remote-doc-manifest#t0013)</li></ul> |

See [EARL results from the JSON-LD 1.1 Test Suite](https://w3c.github.io/json-ld-api/reports/#subj_Titanium_JSON_LD_Java) for more details.

## CLI

[LD-CLI](https://github.com/filip26/ld-cli) is a native command line utility for Ubuntu, Mac, Windows

## Extensions

- [JSON-LD-star](https://json-ld.github.io/json-ld-star) expansion and compaction built-in support (experimental)
- [Universal RDF Dataset Normalization Algorithm - URDNA2015](https://github.com/simon-greatrix/rdf-urdna)

## Usage

### Installation

#### Titanium

Maven


```xml
<!-- Java 11 -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>titanium-json-ld</artifactId>
    <version>1.3.1</version>
</dependency>

```

or

```xml
<!-- Android (Java 8, OkHtttp) -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>titanium-json-ld-jre8</artifactId>
    <version>1.3.1</version>
</dependency>
```

Gradle

```gradle
compile group: 'com.apicatalog', name: 'titanium-json-ld', version: '1.3.1'
```

#### JSON-P Provider

Add JSON-P provider, if it is not on the classpath already.

Maven

```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>jakarta.json</artifactId>
    <version>2.0.1</version>
</dependency>
```

Gradle

```gradle
compile group: 'org.glassfish', name: 'jakarta.json', version: '2.0.1'

```

### Documentation

[![javadoc](https://javadoc.io/badge2/com.apicatalog/titanium-json-ld/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/titanium-json-ld)

### Examples

Titanium provides high-level [JsonLd](https://javadoc.io/doc/com.apicatalog/titanium-json-ld/latest/com/apicatalog/jsonld/JsonLd.html) API to interact with the processor.

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
Document document = JsonDocument.of(InputStream) or JsonDocument.of(Reader) ...

JsonLd.expand(document).get();

JsonLd.compact(document, contextDocument).get();
...
```

## Contributing

All PR's welcome!

- develop
  - implement a new feature 
  - fix an existing issue
  - improve an existing implementation
- test
  - report a bug
  - implement a test case
- document
  - write javadoc
  - write a tutorial
  - proofread an existing documentation
- promote
  - star, share, the project
  - write an article
- sponsor
  - your requests get top priority
  - you will get a badge

### Building

Fork and clone the project repository.

#### Java 11

```bash
> cd titanium-json-ld
> ./mvnw clean package
```

#### Java 8

```bash
> cd titanium-json-ld
> ./mvnw -f pom_jre8.xml clean package
```

## Resources
- [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)
- [JSON-LD 1.1 Processing Algorithms and API](https://www.w3.org/TR/json-ld-api/)
- [JSON-LD 1.1 Framing](https://www.w3.org/TR/json-ld-framing/)
- [JSON-LD Best Practices](https://w3c.github.io/json-ld-bp/)
- [JSON-LD-star](https://json-ld.github.io/json-ld-star/)
- [JSON-LD Playground](https://json-ld.org/playground/)

## Commercial Support
Commercial support is available at filip26@gmail.com

