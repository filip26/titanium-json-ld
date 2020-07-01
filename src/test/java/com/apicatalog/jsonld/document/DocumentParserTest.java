package com.apicatalog.jsonld.document;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import javax.json.Json;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.http.media.MediaType;

public class DocumentParserTest {

    @Test
    public void test1() throws JsonLdError {
        
        Document document = DocumentParser.parse(MediaType.N_QUADS, new ByteArrayInputStream("_:b0 <https://example.org> _:b2 . ".getBytes()));
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getRdfContent().isPresent());
    }

    @Test
    public void test2() throws JsonLdError {
        
        Document document = DocumentParser.parse(MediaType.JSON_LD, new ByteArrayInputStream(Json.createObjectBuilder().add("x", 10).build().toString().getBytes()));
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getRdfContent().isEmpty());
    }
    
    @Test
    public void test3() throws JsonLdError {
        
        Document document = DocumentParser.parse(MediaType.N_QUADS, new InputStreamReader(new ByteArrayInputStream("_:b0 <https://example.org> _:b2 . ".getBytes())));
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getRdfContent().isPresent());
    }

    @Test
    public void test4() throws JsonLdError {
        
        Document document = DocumentParser.parse(MediaType.JSON_LD, new InputStreamReader(new ByteArrayInputStream(Json.createObjectBuilder().add("x", 10).build().toString().getBytes())));
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
        Assert.assertTrue(document.getJsonContent().isPresent());
        Assert.assertTrue(document.getRdfContent().isEmpty());
    }

    @Test
    public void testI1() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(null, (InputStream)null));
    }

    @Test
    public void testI2() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(null, (Reader)null));
    }
    
    @Test
    public void testI3() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(MediaType.JSON, (InputStream)null));
    }

    @Test
    public void testI4() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> DocumentParser.parse(MediaType.N_QUADS, (Reader)null));
    }
    
    @Test
    public void testI5() throws JsonLdError {
        final InputStream inputStream = new ByteArrayInputStream("{}".getBytes());
        Assert.assertThrows(JsonLdError.class, () -> DocumentParser.parse(MediaType.HTML, inputStream));
    }

    @Test
    public void testI6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("{}".getBytes()));
        Assert.assertThrows(JsonLdError.class, () -> DocumentParser.parse(MediaType.XHTML, reader));
    }
}
