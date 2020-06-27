package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteDocument;

public class FlatteningApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.flatten((RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.flatten((String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.flatten((URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.flatten("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.flatten("   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.flatten("/relative");
    }
    
    @Test(expected = IllegalArgumentException.class)    
    public void test7() {
        JsonLd.flatten(URI.create("relative"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.flatten("https://example.com").options(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.flatten("https://example.com").base("!!");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.flatten("https://example.com").context("#!");

    }
}
