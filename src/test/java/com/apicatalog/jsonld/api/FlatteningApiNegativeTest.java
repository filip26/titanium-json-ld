package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.impl.FlatteningApi;
import com.apicatalog.jsonld.document.JsonDocument;

public class FlatteningApiNegativeTest {

    @Test
    public void test1() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((JsonDocument)null));
    }

    @Test
    public void test2() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((String)null));
    }
    
    @Test
    public void test3() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten((URI)null));
    }
    
    @Test
    public void test4() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten(""));
    }

    @Test
    public void test5() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten("   "));
    }
    
    @Test
    public void test6() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten("/relative"));
    }
    
    @Test    
    public void test7() {
        final URI uri = URI.create("relative");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.flatten(uri));
    }

    @Test
    public void test10() {
        final FlatteningApi api = JsonLd.flatten("https://example.com");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.options(null));
    }
    
    @Test
    public void test11() {
        final FlatteningApi api = JsonLd.flatten("https://example.com");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.base("!!"));
    }
    
    @Test
    public void test12() {
        final FlatteningApi api = JsonLd.flatten("https://example.com");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.context("#!"));

    }
}
