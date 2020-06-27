package com.apicatalog.jsonld.api;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.net.URI;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;

public class FlatteningApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Json.createArrayBuilder().build());
    
    @Test    
    public void test1() throws JsonLdError {
        JsonStructure result = JsonLd.flatten(JsonDocument.of(Json.createObjectBuilder().build())).get();
        
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createArrayBuilder().build(), result);
    }
    
    @Test    
    public void test2() throws JsonLdError {
        JsonStructure result = JsonLd.flatten(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes()))).get();
        
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createArrayBuilder().build(), result);
    }
    
    @Test    
    public void test3() throws JsonLdError {
        JsonStructure result = JsonLd.flatten("https://example.com").loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createArrayBuilder().build(), result);
    }

    @Test    
    public void test4() throws JsonLdError {
        JsonStructure result = JsonLd.flatten(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createArrayBuilder().build(), result);
    }

    @Test    
    public void test5() throws JsonLdError {
        JsonStructure result = JsonLd.flatten("\thttps://example.com  ").loader(MOCK_LOADER).get();
        
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createArrayBuilder().build(), result);
    }
    
    @Test    
    public void test6() throws JsonLdError {
        JsonStructure result = JsonLd.flatten("https://example.com").context(Document.of(Json.createObjectBuilder().build())).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createObjectBuilder().build(), result);
    }    

    @Test    
    public void test7() throws JsonLdError {
        JsonStructure result = JsonLd.flatten("https://example.com").context(Document.of(MediaType.JSON, new InputStreamReader(new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes())))).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createObjectBuilder().build(), result);
    }    

    @Test    
    public void test8() throws JsonLdError {
        JsonStructure result = JsonLd.flatten("https://example.com").context(Json.createObjectBuilder().build()).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(Json.createObjectBuilder().build(), result);
    }    

}
