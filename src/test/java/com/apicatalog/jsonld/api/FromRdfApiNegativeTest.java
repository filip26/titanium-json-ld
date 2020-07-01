package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.Json;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;

public class FromRdfApiNegativeTest {

    @Test
    public void test1() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf((URI)null));
    }

    @Test
    public void test2() {
        final Document document = JsonDocument.of(Json.createArrayBuilder().build());
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf(document));
    }
    
    @Test
    public void test3() {
        Assert.assertThrows(IllegalArgumentException.class, () -> JsonLd.fromRdf((RdfDocument)null));
    }
}
