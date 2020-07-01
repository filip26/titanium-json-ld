package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.impl.ExpansionApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.Rdf;

public class ExpansionApiNegativeTest {

    @Test
    public void test1() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((JsonDocument)null));
    }

    @Test
    public void test2() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((String)null));
    }
    
    @Test
    public void test3() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand((URI)null));
    }
    
    @Test    
    public void test4() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(""));
    }

    @Test
    public void test5() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand("   "));
    }
    
    @Test
    public void test6() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand("/relative"));
    }
    
    @Test
    public void test7() {
        final URI uri = URI.create("relative");
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(uri));
    }

    @Test
    public void test8() {
        final Document document = RdfDocument.of(Rdf.createDataset());
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.expand(document));
    }

    @Test
    public void test10() {
        final ExpansionApi api = JsonLd.expand("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.base("!//"));
    }
    
    @Test
    public void test11() {
        final ExpansionApi api = JsonLd.expand("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.context("~"));
    }
    
    @Test
    public void test12() {
        final ExpansionApi api = JsonLd.expand("http://example.org");
        Assert.assertThrows(IllegalArgumentException.class, () -> api.options(null));
    }
}
