package com.apicatalog.jsonld.document;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.http.media.MediaType;

public class JsonDocumentTest {
    
    @Test
    public void test1() {
        Document document = JsonDocument.of(Json.createArrayBuilder().build());
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(Json.createArrayBuilder().build(), document.getJsonContent().get());
    }

    @Test
    public void test2() {
        Document document = JsonDocument.of(MediaType.JSON_LD, Json.createObjectBuilder().build());
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(Json.createObjectBuilder().build(), document.getJsonContent().get());
    }

    @Test
    public void test3() throws JsonLdError {
        Document document = JsonDocument.of(new ByteArrayInputStream(Json.createArrayBuilder().build().toString().getBytes()));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(Json.createArrayBuilder().build(), document.getJsonContent().get());
    }

    @Test
    public void test4() throws JsonLdError {
        Document document = JsonDocument.of(new InputStreamReader(new ByteArrayInputStream(Json.createArrayBuilder().build().toString().getBytes())));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(Json.createArrayBuilder().build(), document.getJsonContent().get());
    }

    @Test
    public void test5() {
        Document document = JsonDocument.of(MediaType.of("application/custom+json;profile=https://example.org/profile"), Json.createObjectBuilder().build());
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.of("application", "custom+json").match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isEmpty());
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getProfile().isPresent());
        Assert.assertEquals("https://example.org/profile", document.getProfile().get());
        Assert.assertEquals(Json.createObjectBuilder().build(), document.getJsonContent().get());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testi1() throws JsonLdError {
        JsonDocument.of((InputStream)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testi2() {
        JsonDocument.of((JsonStructure)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testi3() throws JsonLdError {
        JsonDocument.of((Reader)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testi4() throws JsonLdError {
        JsonDocument.of(null, new ByteArrayInputStream(Json.createArrayBuilder().build().toString().getBytes()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testi5() {
        JsonDocument.of(null, Json.createObjectBuilder().build());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testi6() throws JsonLdError {
        JsonDocument.of(null, new InputStreamReader(new ByteArrayInputStream(Json.createArrayBuilder().build().toString().getBytes())));
    }
    
    @Test(expected = JsonLdError.class)
    public void testi7() throws JsonLdError {
        JsonDocument.of(new ByteArrayInputStream("{ bad json".getBytes()));
    }

    @Test(expected = JsonLdError.class)
    public void testi8() throws JsonLdError {
        JsonDocument.of(new InputStreamReader(new ByteArrayInputStream("10".getBytes())));
    }
}
