package com.apicatalog.jsonld.api;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.net.URI;

import javax.json.Json;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;

public class ToRdfApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Json.createArrayBuilder().build());
    
    @Test    
    public void test1() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(Json.createObjectBuilder().build())).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
    
    @Test    
    public void test2() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes()))).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
    
    @Test    
    public void test3() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("https://example.com").loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }

    @Test    
    public void test4() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }

    @Test    
    public void test5() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
    
    @Test    
    public void test6() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com  ").context(Json.createObjectBuilder().build()).loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
        
    @Test    
    public void test7() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(Json.createObjectBuilder().build())).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }    

    @Test    
    public void test8() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf("\thttps://example.com").context(JsonDocument.of(MediaType.JSON, new InputStreamReader(new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes())))).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
    
    @Test    
    public void test9() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(Json.createObjectBuilder().build())).context(Json.createObjectBuilder().build()).loader(MOCK_LOADER).get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
        
    @Test    
    public void test10() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(Json.createObjectBuilder().build())).context(JsonDocument.of(Json.createObjectBuilder().build())).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }    

    @Test    
    public void test11() throws JsonLdError {
        RdfDataset result = JsonLd.toRdf(JsonDocument.of(Json.createObjectBuilder().build())).context(JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes()))).loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());
    }
}
