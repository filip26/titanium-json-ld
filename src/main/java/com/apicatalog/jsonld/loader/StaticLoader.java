package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.PolyNode;

/**
 * A {@link DocumentLoader} that resolves JSON-LD documents from an in-memory
 * map of pre-registered resources.
 *
 * <p>
 * {@code StaticLoader} is typically used in environments where remote access is
 * restricted or deterministic document resolution is required — such as unit
 * tests, embedded deployments, or offline processing pipelines.
 * </p>
 *
 * <p>
 * Each URI is explicitly mapped to a {@link Document} or {@link PolyNode}. When
 * {@link #loadDocument(URI, Options)} is invoked, the loader first checks the
 * internal registry. If no entry exists, it can delegate to an optional
 * fallback {@link DocumentLoader}.
 * </p>
 *
 * <p>
 * The loader is immutable and created through the {@link Builder}.
 * </p>
 *
 * @see DocumentLoader
 * @see Document
 * @see SchemeRouter
 */
public final class StaticLoader implements DocumentLoader {

    private final Map<URI, Document> resources;
    private final DocumentLoader loader;

    private StaticLoader(Map<URI, Document> resources, DocumentLoader loader) {
        this.resources = resources;
        this.loader = loader;
    }

    /** Creates a new empty builder. */
    public static final Builder newBuilder() {
        return newBuilder(Map.of());
    }

    /** Creates a builder initialized from an existing {@code StaticLoader}. */
    public static final Builder newBuilder(StaticLoader loader) {
        return new Builder(loader.resources, loader.loader);
    }

    /** Creates a builder initialized with a predefined resource map. */
    public static final Builder newBuilder(Map<URI, Document> resources) {
        return new Builder(Objects.requireNonNull(resources), null);
    }

    /**
     * Returns the registered document for the given URI or delegates to the
     * fallback loader.
     *
     * @param url     the document URI
     * @param options loading options
     * @return the matching {@link Document}
     * @throws JsonLdException      if no resource is registered for the URI and no
     *                              fallback loader is configured
     * @throws NullPointerException if {@code url} is {@code null}
     */
    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        var document = resources.getOrDefault(
                Objects.requireNonNull(url, "The url must not be null."),
                null);

        if (document != null) {
            return document;
        }

        if (loader != null) {
            return loader.loadDocument(url, options);
        }

        throw new JsonLdException(
                ErrorCode.LOADING_DOCUMENT_FAILED,
                "URL [" + url + "] is not recogized and fallback loader is not set.");
    }

    /**
     * Builder for constructing immutable {@link StaticLoader} instances.
     *
     * <p>
     * Allows registering fixed document mappings and specifying an optional
     * fallback {@link DocumentLoader} to handle unknown URIs.
     * </p>
     */
    public static final class Builder {

        private final Map<URI, Document> resources;
        private DocumentLoader loader;

        Builder(Map<URI, Document> resources, DocumentLoader loader) {
            this.resources = new LinkedHashMap<>(resources);
            this.loader = loader;
        }

        /**
         * Registers a static document for the given URI (string form).
         *
         * @param url      the document URI
         * @param document the document to associate
         * @return this builder instance
         */
        public Builder set(String url, Document document) {
            return set(URI.create(url), document);
        }

        /**
         * Registers a static document for the given URI.
         *
         * @param url      the document URI
         * @param document the document to associate
         * @return this builder instance
         */
        public Builder set(URI url, Document document) {
            resources.put(url, document);
            return this;
        }

        /**
         * Registers a JSON-LD structure for the given URI (string form).
         *
         * @param url  the document URI
         * @param node the parsed JSON-LD node
         * @return this builder instance
         */
        public Builder set(String url, PolyNode node) {
            return set(URI.create(url), node);
        }

        /**
         * Registers a JSON-LD structure for the given URI.
         *
         * @param url  the document URI
         * @param node the parsed JSON-LD node
         * @return this builder instance
         */
        public Builder set(URI url, PolyNode node) {
            resources.put(url, Document.of(node, url));
            return this;
        }

        /**
         * Sets the fallback loader used when a URI is not registered locally.
         *
         * @param loader the fallback loader
         * @return this builder instance
         */
        public Builder fallback(DocumentLoader loader) {
            this.loader = loader;
            return this;
        }

        /** Builds an immutable {@link StaticLoader} instance. */
        public StaticLoader build() {
            return new StaticLoader(Map.copyOf(resources), loader);
        }
    }
}
