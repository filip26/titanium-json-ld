package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ToRdfApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.toRdf((RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.toRdf((String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.toRdf((URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.toRdf(URI.create("/relative"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.toRdf("");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.toRdf("   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.toRdf("relative");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.toRdf("http://example.org").base("!//");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.toRdf("http://example.org").context("~");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.toRdf("http://example.org").options(null);
    }
}
