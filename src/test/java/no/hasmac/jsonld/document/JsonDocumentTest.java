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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.http.media.MediaType;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

class JsonDocumentTest {

    @Test
    void test1() {
        Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        assertNotNull(document);
        assertTrue(MediaType.JSON.match(document.getContentType()));
        assertFalse(document.getRdfContent().isPresent());
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    void test2() {
        Document document = JsonDocument.of(MediaType.JSON_LD, JsonValue.EMPTY_JSON_OBJECT);
        assertNotNull(document);
        assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        assertFalse(document.getRdfContent().isPresent());
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, document.getJsonContent().get());
    }

    @Test
    void test3() throws JsonLdError {
        Document document = JsonDocument.of(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        assertNotNull(document);
        assertTrue(MediaType.JSON.match(document.getContentType()));
        assertFalse(document.getRdfContent().isPresent());
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    void test4() throws JsonLdError {
        Document document = JsonDocument.of(new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes())));
        assertNotNull(document);
        assertTrue(MediaType.JSON.match(document.getContentType()));
        assertFalse(document.getRdfContent().isPresent());
        assertTrue(document.getJsonContent().isPresent());
        assertFalse(document.getProfile().isPresent());
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    void test5() {
        Document document = JsonDocument.of(MediaType.of("application/custom+json;profile=https://example.org/profile"), JsonValue.EMPTY_JSON_OBJECT);
        assertNotNull(document);
        assertTrue(MediaType.of("application", "custom+json").match(document.getContentType()));
        assertFalse(document.getRdfContent().isPresent());
        assertTrue(document.getJsonContent().isPresent());
        assertTrue(document.getProfile().isPresent());
        assertEquals("https://example.org/profile", document.getProfile().get());
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, document.getJsonContent().get());
    }

    @Test
    void testi1() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((InputStream)null));
    }

    @Test
    void testi2() {
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((JsonStructure)null));
    }

    @Test
    void testi3() throws JsonLdError {
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((Reader)null));
    }

    @Test
    void testi4() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes());
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, is));
    }

    @Test
    void testi5() {
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, JsonValue.EMPTY_JSON_OBJECT));
    }

    @Test
    void testi6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, reader));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{ bad json", "   ", "true"})
    void testi7(String content) throws JsonLdError {
        final InputStream is = new ByteArrayInputStream(content.getBytes());
        assertThrows(JsonLdError.class, () -> JsonDocument.of(is));
    }

    @Test
    void testi8() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("n".getBytes()));
        assertThrows(JsonLdError.class, () -> JsonDocument.of(reader));
    }
}
