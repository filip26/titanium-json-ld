# Titanium JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java.

### ✨ Features

- Full conformance with the JSON-LD 1.1 specification  
- Secure, stable, high-performance implementation with ~3000 tests
- Minimal external dependencies
- Simple, easy-to-use API
- Extensions (since v2.0):
  - JSON-LD-star expansion (experimental)
  - Processing policies: undefined terms, dropped values, time/space constraints
  - Generated TypeMap / TermMap (experimental)
  - Built-in loaders: HTTPS, file, classpath, scheme router, URI rewriter, LRUCache, in-memory
  - Generalized processing with [tree-io](https://github.com/filip26/tree-io) (Jakarta JSON, Jackson, or anything else)
    
### 🎯 Status

[![Java 17 CI](https://github.com/filip26/titanium-json-ld/actions/workflows/java17-build.yml/badge.svg)](https://github.com/filip26/titanium-json-ld/actions/workflows/java17-build.yml)
[![CodeQL](https://github.com/filip26/titanium-json-ld/actions/workflows/codeql.yml/badge.svg)](https://github.com/filip26/titanium-json-ld/actions/workflows/codeql.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/c530c6b43b0243c08ce81521c5b4cf6a)](https://app.codacy.com/gh/filip26/titanium-json-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/c530c6b43b0243c08ce81521c5b4cf6a)](https://app.codacy.com/gh/filip26/titanium-json-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_coverage)
[![Javadoc](https://javadoc.io/badge2/com.apicatalog/titanium-json-ld/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/titanium-json-ld)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/titanium-json-ld.svg?label=Maven%20Central)](https://mvnrepository.com/artifact/com.apicatalog/titanium-json-ld)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)



### 🧩 Libraries & Tools

- [LD-CLI](https://github.com/filip26/ld-cli) - A native command line utility for Ubuntu, Mac, and Windows
- [Titanium RDFC](https://github.com/filip26/titanium-rdfc) - W3C Standard RDF Dataset Canonicalization
- [Titanium N-QUADS](https://github.com/filip26/titanium-rdf-n-quads) - W3C RDF 1.1 N-Quads
- [Titanium JCS](https://github.com/filip26/titanium-jcs) - RFC 8785 JSON Canonicalization Scheme (JCS)
- [Iridium CBOR-LD](https://github.com/filip26/iridium-cbor-ld) - CBOR-based Processor for Linked Data

## ✅ Conformance

| Feature | Tests | Pass | Pass Rate | Notes |
| :-- | --: | --: | --: | :-- |
| [Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 376 | 376 | 100% | |
| [Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 244 | 244 | 100% | |
| [Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | 55 | 55 | 100% | |
| [JSON-LD to RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 456 | 454 | 99.6% | <ul><li>[te075 - @vocab as blank node identifier](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#te075)</li><li>[tli12 - List with bad @base](https://w3c.github.io/json-ld-api/tests/toRdf-manifest#tli12)</li></ul> |
| [RDF to JSON-LD](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | 52 | 52 | 100% | |
| [Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | 91 | 90 | 98.9% | <ul><li>[t0059 - @embed: @last](https://w3c.github.io/json-ld-framing/tests/frame-manifest#t0059)</li></ul> |
| [Remote Document and Context Retrieval](https://www.w3.org/TR/json-ld11-api/#remote-document-and-context-retrieval) | 18 | 17 | 94.4% | <ul><li>[t0013 - HTML document](https://w3c.github.io/json-ld-api/tests/remote-doc-manifest#t0013)</li></ul> |

📄 **See also:** [EARL Results from the JSON-LD 1.1 Test Suite](https://w3c.github.io/json-ld-api/reports/#subj_Titanium_JSON_LD_Java)

## 💡 Examples

Titanium provides a high-level [`JsonLd`](https://javadoc.io/doc/com.apicatalog/titanium-json-ld/latest/com/apicatalog/jsonld/JsonLd.html) API for working with JSON-LD documents.

### Transformations

Perform standard JSON-LD operations such as expansion, compaction, flattening, framing, and conversion from/to RDF. The JSON-LD document to process can be remote or local.

```javascript
// Expansion from a remote JSON-LD document
JsonLd.expand("https://w3c.github.io/json-ld-api/tests/expand/0001-in.jsonld")
      .ordered()
      .get();

// Expansion from a local file with an external context
JsonLd.expand("file:/home/filip/document.json")    // HTTP(S) and File schemes supported
      .context("file:/home/filip/context.jsonld")  // external context
      .get();

// Compaction with a remote context
JsonLd.compact("https://example/expanded.jsonld", "https://example/context.jsonld")
      .compactToRelative(false)  // use absolute IRIs
      .get();

// Flattening a JSON-LD document
JsonLd.flatten("https://example/document.jsonld").get();

// Convert JSON-LD to RDF
JsonLd.toRdf("https://example/document.jsonld").provide(RdfConsumer); 

// RDF Dataset Canonicalization with Titanium RDFC
var canonicalizer = new RdfCanon.create(...);
JsonLd.toRdf("https://example/document.jsonld").provide(canonicalizer);
canonicalizer.provide(RdfConsumer);
// or with N-Quads output
canonicalizer.provide(new NQuadsWriter(...));

// Convert RDF to JSON-LD
var consumer = JsonLd.fromRdf();
consumer.quad(...);  // feed manually or via a reader
(new NquadsReader(...)).provide(consumer);

// Get the final JSON-LD result
consumer.toJsonLd();

// Framing a document
JsonLd.frame("https://example/document.jsonld", "https://example/frame.jsonld").get();
```

### Local JSON Document

Load and process JSON-LD documents directly from an `InputStream` or `Reader`.

```javascript
// Load JSON from InputStream or Reader
Document document = JsonDocument.of(inputStream);
Document context = JsonDocument.of(reader);

// Expand the local document
JsonLd.expand(document).get();

// Compact using a local context
JsonLd.compact(document, context).get();
```

### Processing Timeout [Experimental]

Set a maximum processing duration for JSON-LD operations. The timeout does not include time spent loading external documents.

```javascript
// Terminates processing after the specified duration (excluding DocumentLoader time)
JsonLd.expand(document)
      .timeout(Duration.ofSeconds(5))
      .get();
```

### HTTP Document Loader Timeout

Customize the HTTP loader to apply a read timeout when fetching remote JSON-LD or context documents.

```javascript
// Configure a custom HTTP loader with a 30-second read timeout
static DocumentLoader LOADER = HttpLoader.defaultInstance().timeout(Duration.ofSeconds(30));
...
JsonLd.expand(...).loader(LOADER).get();
```

### Document Caching

Use an LRU-based cache to reuse previously loaded documents and reduce network calls. A cache instance can be shared across multiple operations.

```javascript
// LRU cache for remote documents (capacity = 100)
JsonLd.expand("https://example.com/document.jsonld")
      .loader(new LRUDocumentCache(loader, 100))
      .get();

// Reuse cache across multiple documents
DocumentLoader cachedLoader = new LRUDocumentCache(loader, 100);

JsonLd.expand("https://example.com/document.jsonld").loader(cachedLoader).get();
JsonLd.expand("https://example.com/another-document.jsonld").loader(cachedLoader).get();
```

### Undefined Terms Processing Policy

Define how the processor handles terms not defined in the context. Options include Fail, Warn, or `Ignore` (default).

```javascript
// Define behavior for undefined terms: Fail, Warn, or Ignore (default)
JsonLd.expand(document)
      .undefinedTermsPolicy(Fail)  // or Warn | Ignore
      .get();
```

📘 **Learn more:** See the [Javadoc API Reference](https://javadoc.io/doc/com.apicatalog/titanium-json-ld/latest/) for advanced configuration and usage options.

## ⚙️ Installation

### Titanium

#### Maven (Java 11+)

```xml
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>titanium-json-ld</artifactId>
    <version>1.7.0</version>
</dependency>
```

#### Gradle (Java 8+, Android API Level ≥ 24)

```gradle
implementation("com.apicatalog:titanium-json-ld-jre8:1.7.0")
```

> [!WARNING]
> The upcoming **2.x version** is under active development and requires Java 17 or higher. 2.x milestone releases are provided for **testing and feedback purposes only** and **should not be used in production**.  
>
> Each milestone can introduce **breaking changes**.

### JSON-P Provider

Titanium v1.x.x relies on a **JSON-P (Jakarta JSON Processing)** provider. Ensure that one is available on your classpath.

#### Maven

```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>jakarta.json</artifactId>
    <version>2.0.1</version>
</dependency>
```

#### Gradle

```gradle
implementation("org.glassfish:jakarta.json:2.0.1")
```

## 🤝 Contributing

Contributions of all kinds are welcome — whether it’s code, documentation, testing, or community support! Please open a **pull request** or **issue** to get started.

### 💻 Develop
- Implement a new feature  
- Fix an existing issue or bug  
- Refactor or optimize existing code  

### 🧪 Test
- Report bugs or unexpected behavior  
- Add or improve unit/integration tests  
- Verify milestone builds and provide feedback  

### 📖 Document
- Write or improve **Javadoc** and inline comments  
- Create or update tutorials and usage guides  
- Proofread and improve clarity or accuracy in documentation  

### 🌟 Promote
- Star ⭐ and share the project  
- Write a blog post, article, or social media post about it  
- Help answer questions or guide new contributors  

### 💖 Sponsor
Your support helps sustain development.

### 🏗️ Building

Fork and clone the project repository.

> [!NOTE]
> **Version 2.0 is under active development!**  The new major version 2.0 requires Java 17 and is released in **milestones** before the final release.  
>
> Each milestone can introduce **breaking changes** and is **intended only for testing**.

```bash
> cd titanium-json-ld
> mvn package
```

## 📚 Resources

- [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)
- [JSON-LD 1.1 Processing Algorithms and API](https://www.w3.org/TR/json-ld-api/)
- [JSON-LD 1.1 Framing](https://www.w3.org/TR/json-ld-framing/)
- [JSON-LD Best Practices](https://w3c.github.io/json-ld-bp/)
- [JSON-LD-star](https://json-ld.github.io/json-ld-star/)
- [JSON-LD Playground](https://json-ld.org/playground/)

## 💼 Commercial Support

Commercial support and consulting are available.
For inquiries, please contact: filip26@gmail.com
