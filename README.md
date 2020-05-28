# JSON-LD 1.1 Processor & API

An implementation of the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/) (JSON-based Serialization for Linked Data) specification in Java utilizing [JSONP](https://javaee.github.io/jsonp/) (Java API for JSON Processing).

![Java CI with Maven](https://github.com/filip26/jsonp-ld/workflows/Java%20CI%20with%20Maven/badge.svg)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Conformance

The goal is to pass the [official test suite](https://github.com/w3c/json-ld-api/tree/master/tests) and conform to the [JSON-LD 1.1](https://www.w3.org/TR/json-ld/)  specification.

###  Status

*work in progress*

 Feature | Tests | Pass | Status | Notes
 --- | ---: | ---: | ---: | ---
[Expansion](https://www.w3.org/TR/json-ld/#expanded-document-form) | 378 |  369 | 97.6% | JSON-LD 1.0 (8) and non-normative (1) tests do not pass 
[Compaction](https://www.w3.org/TR/json-ld/#compacted-document-form) | 241 | 168 | 69.7% |  JSON-LD 1.0 (2) tests do not pass 
[Flattening](https://www.w3.org/TR/json-ld/#flattened-document-form) | | | TBD |
[Framing](https://www.w3.org/TR/json-ld11-framing/#framing) | | | TBD |
[RDF](https://www.w3.org/TR/json-ld/#relationship-to-rdf) | | | TBD |

## Roadmap

- [x] 0.1 - [Expansion Algorithms](https://www.w3.org/TR/json-ld11-api/#expansion-algorithms)
- [ ] 0.2 - [Compaction Algorithms](https://www.w3.org/TR/json-ld11-api/#compaction-algorithms)
- [ ] 0.3 - [Flattening Algorithms](https://www.w3.org/TR/json-ld11-api/#flattening-algorithms)
- [ ] 0.4 - [RDF Algorithms](https://www.w3.org/TR/json-ld11-api/#rdf-serialization-deserialization-algorithms)
- [ ] 0.5 - [JSON-LD API](https://www.w3.org/TR/json-ld11-api/#the-application-programming-interface)
- [ ] 0.6 - Issues, code cleaning and optimization
- [ ] 0.7 - [Framing](https://www.w3.org/TR/json-ld11-framing/)
- [ ] 0.8 - Issues, code cleaning and optimization
- [ ] 0.9 - Documented, A+ code
- [ ] 1.0 - GA

## Examples

JSONP-LD implements the [JsonLdProcessor](https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface) interface.

#### Expansion 

```javascript
JsonValue result = JsonLd
                     .createProcessor()
                     .expand(URI.create("https://w3c.github.io/json-ld-api/tests/expand/0001-in.jsonld"));
```

```javascript

JsonLdOptions options = new JsonLdOptions();
// external context
options.setExpandContext(URI.create(...));

JsonValue result = JsonLd
                     .createProcessor()
                     .expand(URI.create(...), options);
```

## Notes
* uses `java.net.URI`

