package com.apicatalog.jsonld.api;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;

public class FromRdfApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.fromRdf(null);
    }
    
}
