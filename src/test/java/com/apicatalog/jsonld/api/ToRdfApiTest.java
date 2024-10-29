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
import java.io.InputStreamReader;
import java.net.URI;
import java.util.concurrent.ExecutionException;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.JsonValue;

class ToRdfApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(JsonValue.EMPTY_JSON_ARRAY);

    @Test
    void test1() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test2() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes()))).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test3() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf("https://example.com").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test4() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test5() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test6() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").context(JsonValue.EMPTY_JSON_OBJECT).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test7() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test8() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(MediaType.JSON, new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes())))).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test9() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonValue.EMPTY_JSON_OBJECT).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test10() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void test11() throws JsonLdError, InterruptedException, ExecutionException {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(JsonValue.EMPTY_JSON_OBJECT)).context(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(JsonValue.EMPTY_JSON_OBJECT.toString().getBytes()))).loader(MOCK_LOADER).ordered().get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }
}
