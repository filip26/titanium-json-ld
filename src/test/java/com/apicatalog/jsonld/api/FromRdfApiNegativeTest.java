package com.apicatalog.jsonld.api;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;

public class FromRdfApiNegativeTest {

    @Test    
    public void test1() {
        try {
            JsonLd.fromRdf(null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
}
