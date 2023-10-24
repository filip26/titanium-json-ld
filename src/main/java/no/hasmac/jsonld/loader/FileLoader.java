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
package no.hasmac.jsonld.loader;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.http.media.MediaType;

public final class FileLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(FileLoader.class.getName());

    private final DocumentResolver resolver;

    public FileLoader() {
        this.resolver = new DocumentResolver();
        this.resolver.setFallbackContentType(MediaType.JSON);
    }

    @Override
    public Document loadDocument(final URI url, final DocumentLoaderOptions options) throws JsonLdError {

        if (!"file".equalsIgnoreCase(url.getScheme())) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unsupported URL scheme [" + url.getScheme() + "]. FileLoader accepts only file scheme.");
        }

        final File file = new File(url);

        if (!file.canRead()) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "File [" + url + "] is not accessible to read.");
        }

        final MediaType contentType =
                                detectedContentType(url.getPath().toLowerCase())
                                .orElseGet(() -> {
                                    LOGGER.log(Level.WARNING, "Cannot detect file [{0}] content type. Trying application/json.", url);
                                    return MediaType.JSON;
                                });

        final DocumentReader<InputStream> reader = resolver.getReader(contentType);

        try (final InputStream is = new FileInputStream(file)) {
            final Document document = reader.read(is);
            document.setDocumentUrl(url);
            return document;

        } catch (FileNotFoundException e) {

            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "File not found [" + url + "].");

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    private static Optional<MediaType> detectedContentType(String name) {

        if (name == null || StringUtils.isBlank(name)) {
            return Optional.empty();
        }

        if (name.endsWith(".nq")) {
            return Optional.of(MediaType.N_QUADS);
        }
        if (name.endsWith(".json")) {
            return Optional.of(MediaType.JSON);
        }
        if (name.endsWith(".jsonld")) {
            return Optional.of(MediaType.JSON_LD);
        }
        if (name.endsWith(".html")) {
            return Optional.of(MediaType.HTML);
        }

        return Optional.empty();
    }
}
