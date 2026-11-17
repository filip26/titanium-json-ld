/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.Document.ContentReader;
import com.apicatalog.web.media.MediaType;

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
 * Each URI is explicitly mapped to a {@link Document} or {@link Tree}. When
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

    private static final Logger LOGGER = Logger.getLogger(StaticLoader.class.getName());

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
    public static final Builder copyOf(StaticLoader loader) {
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
     * @throws LoaderException      if no resource is registered for the URI and no
     *                              fallback loader is configured
     * @throws NullPointerException if {@code url} is {@code null}
     */
    @Override
    public Document loadDocument(URI url, Options options) throws LoaderException {

        var document = resources.getOrDefault(
                Objects.requireNonNull(url, "The url must not be null."),
                null);

        if (document != null) {
            return document;
        }

        if (loader != null) {
            return loader.loadDocument(url, options);
        }
        return null;
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
        private MediaType contentType;
        private Document.ContentReader reader;

        Builder(Map<URI, Document> resources, DocumentLoader loader) {
            this.resources = new LinkedHashMap<>(resources);
            this.loader = loader;
            this.contentType = null;
            this.reader = null;
        }

        /**
         * Registers a static document for the given URI (string form).
         *
         * @param url      the document URI
         * @param document the document to associate
         * @return this builder instance
         */
        public Builder document(String url, Document document) {
            return document(URI.create(url), document);
        }

        /**
         * Registers a static document for the given URI.
         *
         * @param url      the document URI
         * @param document the document to associate
         * @return this builder instance
         */
        public Builder document(URI url, Document document) {
            resources.put(url, document);
            return this;
        }

        /**
         * Registers a JSON-LD structure for the given URI (string form).
         *
         * @param url         the document URI
         * @param contentType
         * @param node        the parsed JSON-LD node
         * @return this builder instance
         */
        Builder node(String url, MediaType contentType, Object node) {
            return node(URI.create(url), contentType, node);
        }

        /**
         * Registers a JSON-LD structure for the given URI.
         *
         * @param url         the document URI
         * @param contentType
         * @param node        the parsed JSON-LD node
         * @return this builder instance
         */
        Builder node(URI url, MediaType contentType, Object node) {
            resources.put(url, new Document(url, node, contentType, null, null));
            return this;
        }

        public Builder reader(MediaType contentType, ContentReader reader) {
            this.contentType = contentType;
            this.reader = reader;
            return this;
        }

        public Builder reader(ContentReader parser) {
            this.reader = parser;
            return this;
        }

        /**
         * 
         * Note: a parser must be set with {@code #parser(TreeParser)} method.
         * 
         * @param url
         * @param resource an absolute classpath starting with '/' pointing to a
         *                 resource
         * @return
         */
        public Builder classpath(String url, String resource) {
            try (final var is = new InputStreamReader(StaticLoader.class.getResourceAsStream(resource),
                    StandardCharsets.UTF_8)) {

                return node(
                        url,
                        contentType != null
                                ? contentType
                                : FileLoader.fromFileExtension(resource), // detect media type
                        reader.read(contentType, is));

            } catch (IOException e) {
                LOGGER.log(Level.SEVERE, "An error [{0}] during loading static context [{1}]",
                        new Object[] { e.getMessage(), resource });
            }
            return this;
        }

        public Builder classpath(Map<String, String> resources) {
            resources.forEach(this::classpath);
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
