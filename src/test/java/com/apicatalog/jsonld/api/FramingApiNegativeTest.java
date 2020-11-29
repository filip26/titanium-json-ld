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

import java.net.URI;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;

import jakarta.json.JsonValue;

public class FramingApiNegativeTest {

    @Test
    public void test1() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((JsonDocument)null, (JsonDocument)null));
    }

    @Test
    public void test2() {
        final Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(document, (JsonDocument)null));
    }
    
    @Test
    public void test3() {
        final Document document = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((JsonDocument)null, document));
    }
    
    @Test
    public void test4() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((String)null, (String)null));
    }

    @Test
    public void test5() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("https://example.org", null));
    }
    
    @Test
    public void test6() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(null, "http://example.com"));
    }
    
    @Test
    public void test7() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("", "http://example.com"));
    }

    @Test
    public void test8() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org/", ""));
    }
    
    @Test
    public void test9() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org", "   "));
    }
    
    @Test
    public void test10() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("http://example.org", "relative"));
    }
    
    @Test
    public void test11() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame("relative", "http://example.org"));
    }
    
    @Test
    public void test12() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame((URI)null, (URI)null));
    }
    
    @Test
    public void test13() {
        final URI uri = URI.create("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(uri, null));
    }
    
    @Test
    public void test14() {
        final URI uri = URI.create("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(null, uri));
    }
    
    @Test
    public void test15() {
        final URI uri1 = URI.create("/relative");
        final URI uri2 = URI.create("http://example.com");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.frame(uri1, uri2));
    }
}
