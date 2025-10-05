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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

class FileLoaderTest {

    @Test
    void testLoadJson() throws URISyntaxException, JsonLdError {

        URL fileUrl = getClass().getResource("document.json");

        assertNotNull(fileUrl);

        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new LoaderOptions());

        assertNotNull(document);
        assertTrue(MediaType.JSON.match(document.getContentType()));
    }

    @Test
    void testLoadJsonLd() throws URISyntaxException, JsonLdError {

        URL fileUrl = getClass().getResource("document.jsonld");

        assertNotNull(fileUrl);

        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new LoaderOptions());

        assertNotNull(document);
        assertTrue(MediaType.JSON_LD.match(document.getContentType()));
    }

    @Test
    void testLoadHtml() throws URISyntaxException {

        URL fileUrl = getClass().getResource("document.html");

        assertNotNull(fileUrl);

        assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(fileUrl.toURI(), new LoaderOptions()));
    }

    @Test
    void testUnsupportedScheme() throws URISyntaxException {
        assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(URI.create("https://github.com/"), new LoaderOptions()));
    }

}
