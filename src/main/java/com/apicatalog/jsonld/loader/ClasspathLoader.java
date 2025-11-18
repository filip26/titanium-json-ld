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
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.web.media.MediaType;

/**
 * A {@link DocumentLoader} that retrieves JSON-LD documents from the
 * application classpath.
 *
 * <p>
 * This loader resolves URIs using the {@code classpath:} scheme and loads
 * corresponding resources available on the application's classpath. It is
 * useful for packaging JSON-LD contexts or frames directly within a JAR or
 * classpath resource bundle.
 * </p>
 *
 * <p>
 * Example URI:
 * </p>
 * 
 * <pre>{@code classpath:/contexts/example.jsonld}</pre>
 */
public final class ClasspathLoader implements DocumentLoader {

    private final Map<MediaType, TreeParser> parsers;
    private final TreeParser defaultParser;
    private final Class<?> baseClass;

    /**
     * Creates a new loader that parses classpath resources using the given reader.
     *
     * @param parsers       the {@link TreeParser} used to parse the loaded resource
     *                      (must not be {@code null})
     * @param defaultParser
     * @param base          the class from which relative classpaths are resolved
     */
    private ClasspathLoader(
            final Map<MediaType, TreeParser> parsers,
            final TreeParser defaultParser,
            final Class<?> base) {
        this.parsers = parsers;
        this.defaultParser = defaultParser;
        this.baseClass = base;
    }
    
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * Loads a JSON-LD document from the classpath.
     *
     * <p>
     * The supplied URI must use the {@code classpath:} scheme and refer to a
     * readable resource. If the resource cannot be found or read, a
     * {@link JsonLdException} is thrown.
     * </p>
     *
     * @param url     the {@code classpath:} URI of the resource to load
     * @param options ignored in this implementation
     * @return a {@link Document} representing the loaded resource
     * @throws JsonLdException if the scheme is unsupported or the resource cannot
     *                         be loaded
     */
    @Override
    public Document loadDocument(URI url, Options options) throws JsonLdException {

        if (!"classpath".equalsIgnoreCase(url.getScheme())) {
            throw new JsonLdException(
                    ErrorCode.LOADING_DOCUMENT_FAILED,
                    "Unsupported URL scheme [" + url.getScheme() + "]. Only classpath: scheme is accepted, url=" + url);
        }

        final var path = url.getPath() != null
                ? url.getPath()
                : url.getSchemeSpecificPart();

        final var contentType = FileLoader.fromFileExtension(path); // detect media type
        
        final var parser = parsers.getOrDefault(contentType, defaultParser);
        
        if (parser == null) {
            throw new JsonLdException(
                    ErrorCode.LOADING_DOCUMENT_FAILED,
                    "Cannot parse content-type=" + contentType + ", uri=" + url + ", base=" + baseClass.getPackageName());            
        }

        try (final var is = baseClass.getResourceAsStream(path)) {

            var node = parser.parse(is);

            return Document.of(
                    node,
                    contentType,
                    url);

        } catch (TreeIOException | IOException e) {
            throw new JsonLdException(
                    ErrorCode.LOADING_DOCUMENT_FAILED,
                    "Document loader failed for uri=" + url + ", base=" + baseClass.getPackageName(),
                    e);
        }
    }

    public static final class Builder {

        private final Map<MediaType, TreeParser> parsers;
        private TreeParser defaultParser;
        private Class<?> baseClass;

        Builder() {
            this.parsers = new HashMap<>();
            this.defaultParser = null;
            this.baseClass = ClasspathLoader.class;
        }

        public Builder parser(MediaType contentType, TreeParser parser) {
            this.parsers.put(contentType, parser);
            return this;
        }
        
        public Builder defaultParser(TreeParser parser) {
            this.defaultParser = parser;
            return this;
        }

        public Builder base(Class<?> baseClass) {
            this.baseClass = baseClass;
            return this;
        }

        /** Builds an immutable {@link ClasspathLoader} instance. */
        public ClasspathLoader build() {
            return new ClasspathLoader(
                    Map.copyOf(parsers),
                    defaultParser,
                    baseClass);
        }
    }
}
