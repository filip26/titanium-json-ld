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
import java.io.InputStream;
import java.net.URI;
import java.util.Map;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.web.media.MediaType;

/**
 * Loads JSON-LD documents directly from the local file system.
 *
 * <p>
 * This loader only supports {@code file:} URIs. It reads the referenced file,
 * parses it with the provided {@link NodeParser}, and wraps the result in a
 * {@link Document}. Files must exist and be readable by the current
 * process.
 * </p>
 *
 * @see DocumentLoader
 * @see HttpLoader
 */
public final class FileLoader implements DocumentLoader {

    private static final Map<String, MediaType> EXTENSIONS = Map.of(
            ".jsonld", MediaType.JSON_LD,
            ".json", MediaType.JSON,
            ".html", MediaType.HTML,
            ".xhtml", MediaType.XHTML,
            ".nq", MediaType.N_QUADS);

    private final NodeParser reader;

    /**
     * Creates a loader that parses local files using the given {@link NodeParser}.
     *
     * @param reader parser used to decode the file content
     */
    public FileLoader(NodeParser reader) {
        this.reader = reader;
    }

    /**
     * Resolves and loads a JSON-LD document from a local {@code file:} URI.
     *
     * <p>
     * Only {@code file:} URIs are supported. Other schemes will cause a
     * {@link JsonLdException} to be thrown.
     * </p>
     *
     * @param url     the {@code file:} URI of the document
     * @param options loading options (ignored for file resources)
     * @return the loaded {@link Document}
     * @throws JsonLdException if the file cannot be accessed or parsed
     */
    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        if (!"file".equalsIgnoreCase(url.getScheme())) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Unsupported URL scheme [" + url.getScheme() + "]. FileLoader accepts only file scheme.");
        }

        final File file = new File(url);

        if (!file.canRead()) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "File [" + url + "] is not accessible to read.");
        }

        var contentType = fromFileExtension(file.getName());

        try (final InputStream is = new FileInputStream(file)) {
            var node = reader.parse(is);

            return Document.of(node, contentType, url);

        } catch (FileNotFoundException e) {

            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "File not found [" + url + "].");

        } catch (Exception e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    static final MediaType fromFileExtension(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }

        final String lower = name.toLowerCase();

        return EXTENSIONS.entrySet().stream()
                .filter(e -> lower.endsWith(e.getKey()))
                .map(Map.Entry::getValue)
                .findFirst()
                .orElse(null);
    }
}
