package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.JsonDocument;

public class ExpansionApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.expand((JsonDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.expand((String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.expand((URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)    
    public void test4() {
        JsonLd.expand("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.expand("   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.expand("/relative");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.expand(URI.create("relative"));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.expand("http://example.org").base("!//");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.expand("http://example.org").context("~");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.expand("http://example.org").options(null);
    }
}
