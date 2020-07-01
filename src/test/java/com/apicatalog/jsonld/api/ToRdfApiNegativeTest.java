package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.impl.ToRdfApi;
import com.apicatalog.jsonld.document.JsonDocument;

public class ToRdfApiNegativeTest {

    @Test
    public void test1() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf((JsonDocument)null));
    }

    @Test
    public void test2() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf((String)null));
    }
    
    @Test
    public void test3() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf((URI)null));
    }
    
    @Test
    public void test4() {
        final URI uri = URI.create("/relative");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf(uri));
    }

    @Test
    public void test5() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf(""));
    }
    
    @Test
    public void test6() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf("   "));
    }
    
    @Test
    public void test7() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.toRdf("relative"));
    }

    @Test
    public void test10() {
        final ToRdfApi api = JsonLd.toRdf("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.base("!//"));
    }
    
    @Test
    public void test11() {
        final ToRdfApi api = JsonLd.toRdf("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.context("~"));
    }
    
    @Test
    public void test12() {
        final ToRdfApi api = JsonLd.toRdf("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.options(null));
    }
}
