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
package com.apicatalog.jsonld.document;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.http.media.MediaType;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public class JsonDocumentTest {
    
    @Test
    public void test1() {
        Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    public void test2() {
        Document document = JsonDocument.of(MediaType.JSON_LD, JsonValue.EMPTY_JSON_OBJECT);
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(JsonValue.EMPTY_JSON_OBJECT, document.getJsonContent().get());
    }

    @Test
    public void test3() throws JsonLdError {
        Document document = JsonDocument.of(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    public void test4() throws JsonLdError {
        Document document = JsonDocument.of(new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes())));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(JsonValue.EMPTY_JSON_ARRAY, document.getJsonContent().get());
    }

    @Test
    public void test5() {
        Document document = JsonDocument.of(MediaType.of("application/custom+json;profile=https://example.org/profile"), JsonValue.EMPTY_JSON_OBJECT);
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.of("application", "custom+json").match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isPresent());
        Assert.assertEquals("https://example.org/profile", document.getProfile().get());
        Assert.assertEquals(JsonValue.EMPTY_JSON_OBJECT, document.getJsonContent().get());
    }
    
    @Test
    public void testi1() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((InputStream)null));
    }

    @Test
    public void testi2() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((JsonStructure)null));
    }

    @Test
    public void testi3() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of((Reader)null));
    }
    
    @Test
    public void testi4() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes());
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, is));
    }

    @Test
    public void testi5() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, JsonValue.EMPTY_JSON_OBJECT));
    }

    @Test
    public void testi6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonDocument.of(null, reader));
    }
    
    @Test
    public void testi7() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream("{ bad json".getBytes());
        Assert.assertThrows(JsonLdError.class, () -> JsonDocument.of(is));
    }

    @Test
    public void testi8() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("n".getBytes()));
        Assert.assertThrows(JsonLdError.class, () -> JsonDocument.of(reader));
    }
    
    @Test
    public void testi9() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream("   ".getBytes());
        Assert.assertThrows(JsonLdError.class, () -> JsonDocument.of(is));
    }

    @Test
    public void testi10() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream("true".getBytes());
        Assert.assertThrows(JsonLdError.class, () -> JsonDocument.of(is));
    }
}
