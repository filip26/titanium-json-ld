/**
 * Provides high-level APIs to process JSON-LD documents according to the
 * <a href="https://www.w3.org/TR/json-ld11-api/">JSON-LD 1.1 Processing</a>.
 * <p>
 * The main entry point is {@link com.apicatalog.jsonld.JsonLd}, which offers
 * static methods for expansion, compaction, flattening, framing, and conversion
 * between JSON-LD and RDF representations.
 * </p>
 *
 * <p>
 * Example using a {@link java.util.Map} representation of a JSON-LD document
 * and an {@link com.apicatalog.jsonld.Options}:
 * </p>
 *
 * <pre>{@code
 * import com.apicatalog.jsonld.JsonLd;
 * import com.apicatalog.jsonld.Options;
 * import java.util.Map;
 *
 * var document = Map.of(
 *     "@context", Map.of("name", "http://schema.org/name"),
 *     "name", "Alice"
 * );
 *
 * var options = Options.newOptions()
 *         .base("https://example.com/")
 *         .ordered(true);
 *
 * var expanded = JsonLd.expand(document, options).get();
 * }</pre>
 *
 * @see com.apicatalog.jsonld.JsonLd
 * @see com.apicatalog.jsonld.Options
 */
package com.apicatalog.jsonld;