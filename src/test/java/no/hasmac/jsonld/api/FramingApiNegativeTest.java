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

class FramingApiNegativeTest {

    @Test
    void test1() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((JsonDocument)null, (JsonDocument)null));
    }

    @Test
    void test2() {
        final Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(document, (JsonDocument)null));
    }

    @Test
    void test3() {
        final Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((JsonDocument)null, document));
    }

    @Test
    void test4() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((String)null, (String)null));
    }

    @Test
    void test5() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("https://example.org", (String)null));
    }

    @Test
    void test6() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((String)null, "http://example.com"));
    }

    @Test
    void test7() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("", "http://example.com"));
    }

    @Test
    void test8() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org/", ""));
    }

    @Test
    void test9() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org", "   "));
    }

    @Test
    void test10() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org", "relative"));
    }

    @Test
    void test11() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("relative", "http://example.org"));
    }

    @Test
    void test12() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((URI)null, (URI)null));
    }

    @Test
    void test13() {
        final URI uri = URI.create("http://example.org");
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(uri, (URI)null));
    }

    @Test
    void test14() {
        final URI uri = URI.create("http://example.org");
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((URI)null, uri));
    }

    @Test
    void test15() {
        final URI uri1 = URI.create("/relative");
        final URI uri2 = URI.create("http://example.com");
        assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(uri1, uri2));
    }
}
