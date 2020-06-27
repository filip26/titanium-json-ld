package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.Json;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;

public class FramingApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.frame((Document)null, (Document)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.frame(Document.of(Json.createArrayBuilder().build()), (Document)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.frame((Document)null, Document.of(Json.createArrayBuilder().build()));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.frame((String)null, (String)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.frame("https://example.org", null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.frame(null, "http://example.com");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.frame("", "http://example.com");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        JsonLd.frame("http://example.org/", "");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test9() {
        JsonLd.frame("http://example.org", "   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.frame("http://example.org", "relative");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.frame("relative", "http://example.org");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.frame((URI)null, (URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test13() {
        JsonLd.frame(URI.create("http://example.org"), null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test14() {
        JsonLd.frame(null, URI.create("http://example.org"));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test15() {
        JsonLd.frame(URI.create("/relative"), URI.create("http://example.com"));
    }
}
