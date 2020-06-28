package com.apicatalog.jsonld.api;

import java.io.ByteArrayInputStream;
import java.net.URI;

import javax.json.Json;
import javax.json.JsonObject;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.lang.Version;

public class CompactionApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Json.createArrayBuilder().build());
    
    @Test    
    public void test1() throws JsonLdError {
        JsonObject compacted = JsonLd.compact(JsonDocument.of(Json.createObjectBuilder().build()), JsonDocument.of(Json.createObjectBuilder().build())).get();
        Assert.assertNotNull(compacted);
        Assert.assertEquals(Json.createObjectBuilder().build(), compacted);
    }
    
    @Test    
    public void test2() throws JsonLdError {
        JsonObject compacted = JsonLd.compact(
                JsonDocument.of(MediaType.JSON, new ByteArrayInputStream(Json.createObjectBuilder().build().toString().getBytes())),
                JsonDocument.of(Json.createObjectBuilder().build())
                        ).get();
        Assert.assertNotNull(compacted);
        Assert.assertEquals(Json.createObjectBuilder().build(), compacted);
    }
    
    @Test    
    public void test3() throws JsonLdError {
        JsonObject compacted = JsonLd.compact("https://example.com", JsonDocument.of(Json.createObjectBuilder().build())).loader(MOCK_LOADER).base("").get();
        Assert.assertNotNull(compacted);
        Assert.assertEquals(Json.createObjectBuilder().build(), compacted);
    }

    @Test    
    public void test4() throws JsonLdError {
        JsonObject compacted = JsonLd.compact(URI.create("https://example.com"), JsonDocument.of(Json.createObjectBuilder().build())).loader(MOCK_LOADER).mode(Version.V1_0).get();
        Assert.assertNotNull(compacted);
        Assert.assertEquals(Json.createObjectBuilder().build(), compacted);
    }

    @Test    
    public void test5() throws JsonLdError {
        JsonObject compacted = JsonLd.compact("\thttps://example.com  ", "https://ahoj.fk").loader(MOCK_LOADER).ordered().get();
        Assert.assertNotNull(compacted);
        Assert.assertEquals(Json.createObjectBuilder().build(), compacted);
    }
}
