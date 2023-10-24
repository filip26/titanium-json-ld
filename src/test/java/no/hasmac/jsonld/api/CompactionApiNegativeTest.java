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
package no.hasmac.jsonld.api;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.URI;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.document.JsonDocument;

import jakarta.json.JsonValue;

class CompactionApiNegativeTest {

    public static final Document EMPTY_ARRAY_JSON_DOCUMENT = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);

    @Test
    void test2() {
        assertThrows(
                    IllegalArgumentException.class,
                    () -> JsonLd.compact(EMPTY_ARRAY_JSON_DOCUMENT, (JsonDocument)null));
    }

    @Test
    void test3() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((JsonDocument)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test4() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((JsonDocument)null, (JsonDocument)null));
    }

    @Test
    void test7() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((String)null, (String)null));
    }

    @Test
    void test8() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("http://example.org/", (String)null));
    }

    @Test
    void test9() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((String)null, "http://example.org"));
    }

    @Test
    void test10() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("http://example.org", "relative"));
    }

    @Test
    void test11() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("relative", "http://example.org"));
    }

    @Test
    void test12() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((String)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test13() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("http://example.org", (JsonDocument)null));
    }

    @Test
    void test14() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((String)null, (JsonDocument)null));
    }

    @Test
    void test15() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("/relative", EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test20() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((URI)null, (URI)null));
    }

    @Test
    void test21() {
        final URI uri = URI.create("http://example.com");

        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact(uri, (URI)null));
    }

    @Test
    void test22() {
        final URI uri = URI.create("http://example.com");

        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((URI)null, uri));
    }

    @Test
    void test23() {
        final URI uri1 = URI.create("http://example.com");
        final URI uri2 = URI.create("/relative");

        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact(uri1, uri2));
    }

    @Test
    void test24() {
        final URI uri1 = URI.create("/relative");
        final URI uri2 = URI.create("http://example.com");

        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact(uri1, uri2));
    }

    @Test
    void test25() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((URI)null, (JsonDocument)null));
    }

    @Test
    void test26() {
        final URI uri = URI.create("http://example.com");
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact(uri, (JsonDocument)null));
    }

    @Test
    void test27() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact((URI)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test28() {
        final URI uri = URI.create("relative");
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact(uri, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test29() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("   ", EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    void test30() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("http://example.com", ""));
    }

    @Test
    void test31() {
        assertThrows(
                IllegalArgumentException.class,
                () -> JsonLd.compact("http://example.com", "\t"));
    }
}
