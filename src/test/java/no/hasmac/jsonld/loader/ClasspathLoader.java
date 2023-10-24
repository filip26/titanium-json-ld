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

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.document.RdfDocument;

public class ClasspathLoader implements DocumentLoader, TestLoader {

    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        try (final InputStream is = getClass().getResourceAsStream(url.getPath())) {

            final Document document = toDocument(url, is);
            document.setDocumentUrl(url);

            return document;

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }

    @Override
    public byte[] fetchBytes(URI url) throws JsonLdError {

        try (final InputStream is = getClass().getResourceAsStream(url.getPath())) {

            return ZipResourceLoader.readAsByteArray(is);

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }

    private static Document toDocument(URI url, InputStream is) throws JsonLdError {

        if (url.toString().endsWith(".nq")) {
            return RdfDocument.of(is);
        }

        return JsonDocument.of(is);
    }

}
