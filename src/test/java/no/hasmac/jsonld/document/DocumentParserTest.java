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
package no.hasmac.jsonld.document;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.http.media.MediaType;

import jakarta.json.Json;

class DocumentParserTest {

    @Test
    void test1() throws JsonLdError {

        Document document = DocumentParser.parse(MediaType.N_QUADS, new ByteArrayInputStream("_:b0 <https://example.org> _:b2 . ".getBytes()));

        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertFalse(document.getJsonContent().isPresent());
        assertTrue(document.getRdfContent().isPresent());
    }

    @Test
    void test2() throws JsonLdError {

        Document document = DocumentParser.parse(MediaType.JSON_LD, new ByteArrayInputStream(Json.createObjectBuilder().add("x", 10).build().toString().getBytes()));

        assertNotNull(document);
        assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getRdfContent().isPresent());
    }

    @Test
    void test3() throws JsonLdError {

        Document document = DocumentParser.parse(MediaType.N_QUADS, new InputStreamReader(new ByteArrayInputStream("_:b0 <https://example.org> _:b2 . ".getBytes())));

        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertFalse(document.getJsonContent().isPresent());
        assertTrue(document.getRdfContent().isPresent());
    }

    @Test
    void test4() throws JsonLdError {

        Document document = DocumentParser.parse(MediaType.JSON_LD, new InputStreamReader(new ByteArrayInputStream(Json.createObjectBuilder().add("x", 10).build().toString().getBytes())));

        assertNotNull(document);
        assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getRdfContent().isPresent());
    }

    @Test
    void testI1() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(null, (InputStream)null));
    }

    @Test
    void testI2() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(null, (Reader)null));
    }

    @Test
    void testI3() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(MediaType.JSON, (InputStream)null));
    }

    @Test
    void testI4() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(MediaType.N_QUADS, (Reader)null));
    }

    @Test
    void testI5() throws JsonLdError {
        final InputStream inputStream = new ByteArrayInputStream("{}".getBytes());
        assertThrows(JsonLdError.class, () -> DocumentParser.parse(MediaType.HTML, inputStream));
    }

    @Test
    void testI6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("{}".getBytes()));
        assertThrows(JsonLdError.class, () -> DocumentParser.parse(MediaType.XHTML, reader));
    }
}
