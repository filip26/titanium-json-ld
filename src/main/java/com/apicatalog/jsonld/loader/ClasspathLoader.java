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
import java.io.UncheckedIOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.loader.LoaderException.ErrorCode;

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

    private final Document.ContentReader contentParser;
    private final Class<?> baseClass;

    /**
     * Creates a new loader that parses classpath resources using the given content
     * parser.
     *
     * @param contentParser the {@link Document.ContentReader} used to parse the
     *                      loaded resource (must not be {@code null})
     * @param base          the class from which relative classpaths are resolved
     */
    private ClasspathLoader(
            final Document.ContentReader contentParser,
            final Class<?> base) {
        this.contentParser = contentParser;
        this.baseClass = base;
    }

    /**
     * Creates a new loader that parses classpath resources using the given content
     * parser.
     *
     * @param contentParser the {@link Document.ContentReader} used to parse the
     *                      loaded resource (must not be {@code null})
     * @param base          the class from which relative classpaths are resolved
     */
    public static ClasspathLoader newInstance(
            Document.ContentReader contentParser,
            Class<?> base) {
        return new ClasspathLoader(contentParser, base);
    }

    /**
     * Creates a new loader that parses classpath resources using the given content
     * parser.
     *
     * @param contentParser the {@link Document.ContentReader} used to parse the
     *                      loaded resource (must not be {@code null})
     * @param base          the class from which relative classpaths are resolved
     */
    public static ClasspathLoader newInstance(Document.ContentReader contentParser) {
        return newInstance(contentParser, null);
    }

    /**
     * Loads a JSON-LD document from the classpath.
     *
     * <p>
     * The supplied URI must use the {@code classpath:} scheme and refer to a
     * readable resource. If the resource cannot be found or read, a
     * {@link LoaderException} is thrown.
     * </p>
     *
     * @param uri     the {@code classpath:} URI of the resource to load
     * @param options ignored in this implementation
     * @return a {@link Document} representing the loaded resource
     * @throws LoaderException if the scheme is unsupported or the resource cannot
     *                         be loaded
     */
    @Override
    public Document loadDocument(URI uri, Options options) throws LoaderException {

        if (!"classpath".equalsIgnoreCase(uri.getScheme())) {
            throw new LoaderException(
                    ErrorCode.UNSUPPORTED_SCHEME,
                    uri,
                    "Unsupported URL scheme [" + uri.getScheme() + "]. Only classpath: scheme is accepted, url=" + uri);
        }

        final var path = uri.getPath() != null
                ? uri.getPath()
                : uri.getSchemeSpecificPart();

        final var contentType = FileLoader.fromFileExtension(path); // detect media type

//        if (parser == null) {
//            throw new LoaderException(
//                    ErrorCode.UNSUPPORTED_CONTENT_TYPE,
//                    uri,
//                    "Cannot parse content-type=" + contentType + ", uri=" + uri + ", base="
//                            + baseClass.getPackageName());
//        }

        try (var reader = new InputStreamReader(baseClass.getResourceAsStream(path), StandardCharsets.UTF_8)) {

            var node = contentParser.read(contentType, reader);

            return new Document(
                    uri,
                    node,
                    contentType,
                    null,
                    null);

        } catch (IOException | UncheckedIOException e) {
            throw new LoaderException(
                    uri,
                    "Document loader failed for uri=" + uri + ", base=" + baseClass.getPackageName(),
                    e);
        }
    }
}
