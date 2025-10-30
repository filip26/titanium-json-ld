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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.LoaderOptions;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.nquads.NQuadsReaderException;

public class ClasspathLoader implements DocumentLoader, TestLoader {

    @Override
    public Document loadDocument(URI url, LoaderOptions options) throws JsonLdException {

        try (final InputStream is = getClass().getResourceAsStream(url.getPath())) {

            final Document document = toDocument(url, is);
            document.setDocumentUrl(url);

            return document;

        } catch (IOException | NQuadsReaderException | RdfConsumerException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }

    @Override
    public byte[] fetchBytes(URI url) throws JsonLdException {

        try (final InputStream is = getClass().getResourceAsStream(url.getPath())) {

            return ZipResourceLoader.readAsByteArray(is);

        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
        }
    }

    private static final Document toDocument(URI url, InputStream is) throws JsonLdException, NQuadsReaderException, RdfConsumerException {

        if (url.toString().endsWith(".nq")) {
            return QuadSetDocument.readNQuads(new InputStreamReader(is));
        }

        return JsonDocument.of(is);
    }

}
