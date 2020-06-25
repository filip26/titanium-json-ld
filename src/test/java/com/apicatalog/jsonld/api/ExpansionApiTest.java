package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonArray;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ExpansionApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Json.createArrayBuilder().build());
    
    @Test    
    public void test1() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(RemoteDocument.of(Json.createObjectBuilder().build())).get();
        Assert.assertNotNull(expanded);
        Assert.assertEquals(Json.createArrayBuilder().build(), expanded);
    }
    
    @Test    
    public void test2() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(RemoteDocument.of(Json.createObjectBuilder().build().toString().getBytes())).get();
        Assert.assertNotNull(expanded);
        Assert.assertEquals(Json.createArrayBuilder().build(), expanded);
    }
    
    @Test    
    public void test3() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("https://example.com").loader(MOCK_LOADER).get();
        Assert.assertNotNull(expanded);
        Assert.assertEquals(Json.createArrayBuilder().build(), expanded);
    }

    @Test    
    public void test4() throws JsonLdError {
        JsonArray expanded = JsonLd.expand(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        Assert.assertNotNull(expanded);
        Assert.assertEquals(Json.createArrayBuilder().build(), expanded);
    }

    @Test    
    public void test5() throws JsonLdError {
        JsonArray expanded = JsonLd.expand("\thttps://example.com  ").loader(MOCK_LOADER).get();
        Assert.assertNotNull(expanded);
        Assert.assertEquals(Json.createArrayBuilder().build(), expanded);

    }    
}
