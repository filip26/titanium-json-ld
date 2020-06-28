package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonArray;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.Rdf;

public class FromRdfApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Rdf.createDataset());
    
    @Test    
    public void test1() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf(RdfDocument.of(Rdf.createDataset())).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
        
    @Test    
    public void test3() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf("https://example.com").loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }

    @Test    
    public void test4() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }

    @Test    
    public void test5() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf("\thttps://example.com  ").loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
}
