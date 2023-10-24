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
import no.hasmac.jsonld.document.JsonDocument;

class FlatteningApiNegativeTest {

    @Test
    void test1() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((JsonDocument)null));
    }

    @Test
    void test2() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((String)null));
    }

    @Test
    void test3() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((URI)null));
    }

    @Test
    void test4() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten(""));
    }

    @Test
    void test5() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten("   "));
    }

    @Test
    void test6() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten("/relative"));
    }

    @Test
    void test7() {
        final URI uri = URI.create("relative");
        assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten(uri));
    }

    @Test
    void test10() {
        final FlatteningApi api = JsonLd.flatten("https://example.com");
        assertThrows(IllegalArgumentException.class, () -> api.options(null));
    }
}
