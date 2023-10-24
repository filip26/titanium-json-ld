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
import java.net.URI;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLd;
import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdVersion;
import no.hasmac.jsonld.document.JsonDocument;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

class FrameApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(JsonValue.EMPTY_JSON_OBJECT);

    @Test
    void test1() throws JsonLdError {
        JsonObject framed = JsonLd.frame(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT), JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
        assertNotNull(framed);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, framed);
    }

    @Test
    void test2() throws JsonLdError {
        JsonObject framed = JsonLd.frame(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes())),
                JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT))
                .context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT))
                .get();
        assertNotNull(framed);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, framed);
    }

    @Test
    void test3() throws JsonLdError {
        JsonObject framed = JsonLd.frame("https://example.com", "https://example.com/frame").loader(MOCK_LOADER).base("").get();
        assertNotNull(framed);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, framed);
    }

    @Test
    void test4() throws JsonLdError {
        JsonObject framed = JsonLd.frame(URI.create("https://example.com"), URI.create("https://example.com/frame")).loader(MOCK_LOADER).mode(JsonLdVersion.V1_0).get();
        assertNotNull(framed);
        assertEquals(Json.createObjectBuilder().add(Keywords.GRAPH, JsonValue.EMPTY_JSON_ARRAY).build(), framed);
    }
}
