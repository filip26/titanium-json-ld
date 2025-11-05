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

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdTestSuite;
import com.apicatalog.web.media.MediaType;

class FileLoaderTest {

    @Test
    void testLoadJson() throws URISyntaxException, JsonLdException {

        URL fileUrl = getClass().getResource("document.json");

        assertNotNull(fileUrl);

        Document document = JsonLdTestSuite.FILE_LOADER.loadDocument(fileUrl.toURI(), DocumentLoader.defaultOptions());

        assertNotNull(document);
        assertNotNull(document.content());
        assertTrue(MediaType.JSON.match(document.contentType()));
    }

    @Test
    void testLoadJsonLd() throws URISyntaxException, JsonLdException {

        URL fileUrl = getClass().getResource("document.jsonld");

        assertNotNull(fileUrl);

        Document document = JsonLdTestSuite.FILE_LOADER.loadDocument(fileUrl.toURI(), DocumentLoader.defaultOptions());

        assertNotNull(document);
        assertTrue(MediaType.JSON_LD.match(document.contentType()));
    }

    @Test
    void testLoadHtml() throws URISyntaxException {

        URL fileUrl = getClass().getResource("document.html");

        assertNotNull(fileUrl);

        assertThrows(JsonLdException.class, () -> JsonLdTestSuite.FILE_LOADER.loadDocument(fileUrl.toURI(), DocumentLoader.defaultOptions()));
    }

    @Test
    void testUnsupportedScheme() throws URISyntaxException {
        assertThrows(JsonLdException.class, () -> JsonLdTestSuite.FILE_LOADER.loadDocument(URI.create("https://github.com/"), DocumentLoader.defaultOptions()));
    }

}
