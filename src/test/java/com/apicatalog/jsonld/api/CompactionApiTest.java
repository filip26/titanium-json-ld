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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.concurrent.ExecutionException;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;

import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

class CompactionApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(JsonValue.EMPTY_JSON_ARRAY);

    @Test
    void test1() throws JsonLdError, InterruptedException, ExecutionException {
        JsonObject compacted = JsonLd.compact(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT), JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
        assertNotNull(compacted);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, compacted);
    }

    @Test
    void test2() throws JsonLdError, InterruptedException, ExecutionException {
        JsonObject compacted = JsonLd.compact(
                JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes())),
                JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)
                        ).get();
        assertNotNull(compacted);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, compacted);
    }

    @Test
    void test3() throws JsonLdError, InterruptedException, ExecutionException {
        JsonObject compacted = JsonLd.compact("https://example.com", JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).base("").get();
        assertNotNull(compacted);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, compacted);
    }

    @Test
    void test4() throws JsonLdError, InterruptedException, ExecutionException {
        JsonObject compacted = JsonLd.compact(URI.create("https://example.com"), JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).mode(JsonLdVersion.V1_0).get();
        assertNotNull(compacted);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, compacted);
    }

    @Test
    void test5() throws JsonLdError, InterruptedException, ExecutionException {
        JsonObject compacted = JsonLd.compact("\thttps://example.com  ", "https://ahoj.fk").loader(MOCK_LOADER).ordered().get();
        assertNotNull(compacted);
        assertEquals(JsonValue.EMPTY_JSON_OBJECT, compacted);
    }
}
