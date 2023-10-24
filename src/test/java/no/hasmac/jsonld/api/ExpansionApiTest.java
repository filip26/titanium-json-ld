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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.net.URI;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.http.media.MediaType;

import jakarta.json.JsonArray;
import jakarta.json.JsonValue;

class ExpansionApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(JsonValue.EMPTY_JSON_ARRAY);

    @Test
    void test1() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test2() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes()))).get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test3() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("https://example.com").loader(MOCK_LOADER).base("").get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test4() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(URI.create("https://example.com")).loader(MOCK_LOADER).mode(JsonLdVersion.V1_0).get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test5() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("\thttps://example.com  ").loader(MOCK_LOADER).ordered().get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test6() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("\thttps://example.com").context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).ordered().get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test7() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("\thttps://example.com").context(JsonDocument.of(MediaType.JSON, new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes())))).loader(MOCK_LOADER).ordered().get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test8() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("\thttps://example.com").context(JsonValue.EMPTY_JSON_OBJECT).loader(MOCK_LOADER).ordered().get();
        assertNotNull(expanded);
        assertEquals(JsonValue.EMPTY_JSON_ARRAY, expanded);
    }

    @Test
    void test9() throws JsonLdError {
        ExpansionApi api = JsonLd.expand("file:/example.org").context("file:/example.org");
        assertNotNull(api);
    }

    @Test
    void test10() throws JsonLdError {
        ExpansionApi api = JsonLd.expand("file:///example.org").context("file:///example.org");
        assertNotNull(api);
    }
}
