package com.apicatalog.jsonld.api;

import java.net.URI;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RdfDocument;

public class FromRdfApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.fromRdf((URI)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.fromRdf((RdfDocument)null);
    }
}
