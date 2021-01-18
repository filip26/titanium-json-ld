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
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.DocumentParser;
import com.apicatalog.jsonld.http.media.MediaType;

import static com.apicatalog.jdk8.Jdk8Compatibility.isBlank;

public final class FileLoader implements DocumentLoader {

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
                                .orElseThrow(() -> new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unknown media type of the file [" + url + "]."));

        try (final InputStream is = new FileInputStream(file)) {

            final Document document = DocumentParser.parse(contentType, is);
            document.setDocumentUrl(url);
            return document;

        } catch (FileNotFoundException e) {

            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "File not found [" + url + "].");

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    private static final Optional<MediaType> detectedContentType(String name) {

        if (name == null || isBlank(name)) {
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
