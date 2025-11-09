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

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

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
 * <pre>{@code classpath:/contexts/example.jsonld}</pre>
 */
public final class ClasspathLoader implements DocumentLoader {

    private final TreeParser parser;

    /**
     * Creates a new loader that parses classpath resources using the given reader.
     *
     * @param reader the {@link TreeParser} used to parse the loaded resource (must
     *               not be {@code null})
     */
    public ClasspathLoader(final TreeParser reader) {
        this.parser = reader;
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
                    "Unsupported URL scheme [" + url.getScheme() + "]. Only classpath: scheme is accepted.");
        }

        try (final var is = ClasspathLoader.class.getResourceAsStream(url.getPath())) {

            var node = parser.parse(is);

            return Document.of(
                    node,
                    FileLoader.fromFileExtension(url.getPath()), // detect media type
                    url);

        } catch (TreeIOException | IOException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
}
