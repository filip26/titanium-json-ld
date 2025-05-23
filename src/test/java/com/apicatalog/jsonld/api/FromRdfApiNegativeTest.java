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
package com.apicatalog.jsonld.api;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.URI;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;

import jakarta.json.JsonValue;

@Deprecated
class FromRdfApiNegativeTest {

    @Test
    void test1() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf((URI)null));
    }

    @Test
    void test2() {
        final Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf(document));
    }

    @Test
    void test3() {
        assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf((RdfDocument)null));
    }
}
