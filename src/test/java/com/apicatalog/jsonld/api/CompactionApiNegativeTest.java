package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonValue;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;

public class CompactionApiNegativeTest {

    public static final Document EMPTY_ARRAY_JSON_DOCUMENT = JsonDocument.of(JsonValue.EMPTY_JSON_ARRAY);
    
    @Test
    public void test2() {
        Assert.assertThrows(
                    IllegalArgumentException.class, 
                    () -> JsonLd.compact(EMPTY_ARRAY_JSON_DOCUMENT, (JsonDocument)null));
    }
    
    @Test
    public void test3() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((JsonDocument)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }
    
    @Test
    public void test4() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((JsonDocument)null, (JsonDocument)null));
    }
    
    @Test
    public void test7() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((String)null, (String)null));
    }

    @Test
    public void test8() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("http://example.org/", (String)null));
    }
    
    @Test
    public void test9() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((String)null, "http://example.org"));
    }
    
    @Test
    public void test10() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("http://example.org", "relative"));
    }
    
    @Test
    public void test11() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("relative", "http://example.org"));
    }
    
    @Test
    public void test12() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((String)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }
    
    @Test
    public void test13() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("http://example.org", (JsonDocument)null));
    }
    
    @Test
    public void test14() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((String)null, (JsonDocument)null));
    }
    
    @Test
    public void test15() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("/relative", EMPTY_ARRAY_JSON_DOCUMENT));
    }
    
    @Test
    public void test20() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((URI)null, (URI)null));
    }

    @Test
    public void test21() {
        final URI uri = URI.create("http://example.com");
        
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact(uri, (URI)null));
    }

    @Test
    public void test22() {
        final URI uri = URI.create("http://example.com");
        
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((URI)null, uri));
    }

    @Test
    public void test23() {
        final URI uri1 = URI.create("http://example.com");
        final URI uri2 = URI.create("/relative");

        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact(uri1, uri2));
    }

    @Test
    public void test24() {
        final URI uri1 = URI.create("/relative");
        final URI uri2 = URI.create("http://example.com");
                
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact(uri1, uri2));
    }

    @Test
    public void test25() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((URI)null, (JsonDocument)null));
    }

    @Test
    public void test26() {
        final URI uri = URI.create("http://example.com");
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact(uri, (JsonDocument)null));
    }

    @Test
    public void test27() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact((URI)null, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    public void test28() {
        final URI uri = URI.create("relative");
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact(uri, EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    public void test29() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("   ", EMPTY_ARRAY_JSON_DOCUMENT));
    }

    @Test
    public void test30() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("http://example.com", ""));
    }

    @Test
    public void test31() {
        Assert.assertThrows(
                IllegalArgumentException.class, 
                () -> JsonLd.compact("http://example.com", "\t"));
    }
}
