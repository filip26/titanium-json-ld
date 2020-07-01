package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

public class FileLoaderTest {

    @Test
    public void testLoadNQuads() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.nq");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
    }

    @Test
    public void testLoadJson() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.json");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
    }

    @Test
    public void testLoadJsonLd() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.jsonld");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
    }

    @Test
    public void testLoadHtml() throws URISyntaxException {
        
        URL fileUrl = getClass().getResource("document.html");
        
        Assert.assertNotNull(fileUrl);
        
        Assert.assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(fileUrl.toURI(), new DocumentLoaderOptions()));
    }

    @Test
    public void testUnsupportedScheme() throws URISyntaxException {        
        Assert.assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(URI.create("https://github.com/"), new DocumentLoaderOptions()));
    }

}
