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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.LoaderException.ErrorCode;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.web.media.MediaType;

/**
 * Loads JSON-LD documents directly from the local file system.
 *
 * <p>
 * This loader only supports {@code file:} URIs. It reads the referenced file,
 * parses it with the provided {@link TreeParser}, and wraps the result in a
 * {@link Document}. Files must exist and be readable by the current process.
 * </p>
 *
 * @see DocumentLoader
 * @see HttpLoader
 */
public final class FileLoader implements DocumentLoader {

    private static final Map<String, MediaType> FILE_EXTENSIONS = Map.of(
            ".jsonld", MediaType.JSON_LD,
            ".json", MediaType.JSON,
            ".html", MediaType.HTML,
            ".xhtml", MediaType.XHTML,
            ".nq", MediaType.N_QUADS,
            ".cbor", MediaType.CBOR,
            ".cborld", MediaType.CBOR_LD,
            ".yml", MediaType.YAML,
            ".yaml", MediaType.YAML,
            ".yamlld", MediaType.YAML_LD);

    private final Map<MediaType, TreeParser> parsers;
    private final TreeParser defaultParser;

    private FileLoader(TreeParser defaultParser, Map<MediaType, TreeParser> parsers) {
        this.defaultParser = defaultParser;
        this.parsers = parsers;
    }

    /**
     * Creates a loader that parses local files using the given {@link TreeParser}.
     *
     * @param defaultParser parser used to decode the file content
     */
    public static final FileLoader of(TreeParser parser) {
        return new FileLoader(Objects.requireNonNull(parser), Map.of());
    }

    public static final Builder newBuilder() {
        return new Builder();
    }

    /**
     * Resolves and loads a JSON-LD document from a local {@code file:} URI.
     *
     * <p>
     * Only {@code file:} URIs are supported. Other schemes will cause a
     * {@link LoaderException} to be thrown.
     * </p>
     *
     * @param uri     the {@code file:} URI of the document
     * @param options loading options (ignored for file resources)
     * @return the loaded {@link Document}
     * @throws LoaderException if the file cannot be accessed or parsed
     */
    @Override
    public Document loadDocument(final URI uri, final Options options) throws LoaderException {

        if (!"file".equalsIgnoreCase(uri.getScheme())) {
            throw new LoaderException(
                    ErrorCode.UNSUPPORTED_SCHEME,
                    uri, 
                    "Unsupported URI scheme [" + uri.getScheme() + "]. FileLoader accepts only file scheme.");
        }

        final File file = new File(uri);

        if (!file.canRead()) {
            throw new LoaderException(
                    ErrorCode.FORBIDDEN,
                    uri, 
                    "File [" + uri + "] is not accessible to read.");
        }

        final var contentType = fromFileExtension(file.getName());

        final var parser = parsers.getOrDefault(contentType, defaultParser);

        try (final var is = new FileInputStream(file)) {
            var node = parser.parse(is);

            return Document.of(node, contentType, uri);

        } catch (FileNotFoundException e) {
            throw new LoaderException(ErrorCode.NOT_FOUND, uri, "File not found [" + uri + "].");

        } catch (Exception e) {
            throw new LoaderException(ErrorCode.UNSUPPORTED_CONTENT_TYPE, uri, e);
        }
    }

    static final MediaType fromFileExtension(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }

        final String lower = name.toLowerCase();

        return FILE_EXTENSIONS.entrySet().stream()
                .filter(e -> lower.endsWith(e.getKey()))
                .map(Map.Entry::getValue)
                .findFirst()
                .orElse(null);
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

        private Map<MediaType, TreeParser> parsers;
        private TreeParser defaultParser;

        Builder() {
            this.parsers = new HashMap<>();
            this.defaultParser = null;
        }

        public Builder parser(MediaType contentType, TreeParser parser) {
            this.parsers.put(contentType, parser);
            return this;
        }

        public Builder parser(TreeParser parser) {
            this.defaultParser = parser;
            return this;
        }

        /** Builds an immutable {@link FileLoader} instance. */
        public FileLoader build() {
            return new FileLoader(
                    defaultParser,
                    Map.copyOf(parsers));
        }
    }
}
