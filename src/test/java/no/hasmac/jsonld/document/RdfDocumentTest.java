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

import static org.junit.jupiter.api.Assertions.assertEquals;
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
import no.hasmac.rdf.Rdf;
import no.hasmac.rdf.RdfDataset;

import jakarta.json.JsonValue;

class RdfDocumentTest {

    private static final String NQ_STATEMENT = "<http://example/s> <http://example/p> <http://example/o> <http://example/g> .";

    @Test
    void test1() {
        Document document = RdfDocument.of(Rdf.createDataset());
        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertTrue(document.getRdfContent().isPresent());
        assertFalse(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(0, document.getRdfContent().get().size());
    }

    @Test
    void test2() {
        Document document = RdfDocument.of(MediaType.N_QUADS, Rdf.createDataset());
        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertTrue(document.getRdfContent().isPresent());
        assertFalse(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(0, document.getRdfContent().get().size());
    }

    @Test
    void test3() throws JsonLdError {
        Document document = RdfDocument.of(new ByteArrayInputStream(NQ_STATEMENT.getBytes()));
        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertTrue(document.getRdfContent().isPresent());
        assertFalse(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(1, document.getRdfContent().get().size());
    }

    @Test
    void test4() throws JsonLdError {
        Document document = RdfDocument.of(new InputStreamReader(new ByteArrayInputStream(NQ_STATEMENT.getBytes())));
        assertNotNull(document);
        assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        assertTrue(document.getRdfContent().isPresent());
        assertFalse(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(1, document.getRdfContent().get().size());
    }

    @Test
    void testi1() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((InputStream)null));
    }

    @Test
    void testi2() {
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((RdfDataset)null));
    }

    @Test
    void testi3() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((Reader)null));
    }

    @Test
    void testi4() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes());
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, is));
    }

    @Test
    void testi5() {
        final RdfDataset dataset = Rdf.createDataset();
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, dataset));
    }

    @Test
    void testi6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, reader));
    }

    @Test
    void testi7() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream("{ bad json".getBytes());
        assertThrows(JsonLdError.class, () -> RdfDocument.of(is));
    }

    @Test
    void testi8() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("n".getBytes()));
        assertThrows(JsonLdError.class, () -> RdfDocument.of(reader));
    }

    @Test
    void test9() {
        final MediaType mediaType = MediaType.of("application/custom+json;profile=https://example.org/profile");
        final RdfDataset dataset = Rdf.createDataset();
        assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(mediaType, dataset));
    }
}
