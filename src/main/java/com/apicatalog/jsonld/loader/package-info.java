/**
 * Provides loaders for retrieving remote and local JSON-LD documents.
 *
 * <p>
 * This package contains the {@link com.apicatalog.jsonld.loader.DocumentLoader}
 * interface and its various implementations responsible for resolving JSON-LD
 * documents, contexts, and frames from different sources such as HTTP, files,
 * classpath resources, or in-memory mappings.
 * </p>
 *
 * <p>
 * Loaders can be combined, cached, or routed by URI scheme to support flexible
 * and extensible document retrieval strategies in JSON-LD processing.
 * </p>
 *
 * @see com.apicatalog.jsonld.loader.DocumentLoader
 */
package com.apicatalog.jsonld.loader;