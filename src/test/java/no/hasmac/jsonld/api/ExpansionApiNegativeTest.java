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
import no.hasmac.jsonld.document.RdfDocument;
import no.hasmac.rdf.Rdf;

class ExpansionApiNegativeTest {

    @Test
    void test1() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((JsonDocument)null));
    }

    @Test
    void test2() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((String)null));
    }

    @Test
    void test3() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((URI)null));
    }

    @Test
    void test4() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(""));
    }

    @Test
    void test5() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand("   "));
    }

    @Test
    void test6() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand("/relative"));
    }

    @Test
    void test7() {
        final URI uri = URI.create("relative");
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(uri));
    }

    @Test
    void test8() {
        final Document document = RdfDocument.of(Rdf.createDataset());
        assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(document));
    }

    @Test
    void test9() {
        final ExpansionApi api = JsonLd.expand("http://example.org");
        assertThrows(IllegalArgumentException.class, () -> api.options(null));
    }
}
