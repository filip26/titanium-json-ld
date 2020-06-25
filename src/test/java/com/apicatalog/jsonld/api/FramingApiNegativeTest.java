package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;

public class FramingApiNegativeTest {

    @Test    
    public void test1() {
        try {
            JsonLd.frame((RemoteDocument)null, (RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test2() {
        try {
            JsonLd.frame(RemoteDocument.of(Json.createArrayBuilder().build()), (RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test3() {
        try {
            JsonLd.frame((RemoteDocument)null, RemoteDocument.of(Json.createArrayBuilder().build()));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test4() {
        try {
            JsonLd.frame((String)null, (String)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test5() {
        try {
            JsonLd.frame("https://example.org", null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test6() {
        try {
            JsonLd.frame(null, "http://example.com");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test7() {
        try {
            JsonLd.frame("", "http://example.com");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test8() {
        try {
            JsonLd.frame("http://example.org/", "");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test9() {
        try {
            JsonLd.frame("http://example.org", "   ");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test10() {
        try {
            JsonLd.frame("http://example.org", "relative");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test11() {
        try {
            JsonLd.frame("relative", "http://example.org");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test12() {
        try {
            JsonLd.frame((URI)null, (URI)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test13() {
        try {
            JsonLd.frame(URI.create("http://example.org"), null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test14() {
        try {
            JsonLd.frame(null, URI.create("http://example.org"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test15() {
        try {
            JsonLd.frame(URI.create("/relative"), URI.create("http://example.com"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test16() {
        try {
            JsonLd.frame(new RemoteDocument(new Document() {
                
                @Override
                public boolean isRawPayload() {
                    return false;
                }
                
                @Override
                public boolean isJsonStructure() {
                    return false;
                }
                
                @Override
                public Optional<byte[]> getRawPayload() throws JsonLdError {
                    return null;
                }
                
                @Override
                public Optional<JsonStructure> getJsonStructure() throws JsonLdError {
                    return null;
                }
            }), RemoteDocument.of(Json.createArrayBuilder().build()));
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test17() {
        try {
            JsonLd.frame(
                RemoteDocument.of(Json.createArrayBuilder().build()),
                new RemoteDocument(new Document() {
                
                @Override
                public boolean isRawPayload() {
                    return false;
                }
                
                @Override
                public boolean isJsonStructure() {
                    return false;
                }
                
                @Override
                public Optional<byte[]> getRawPayload() throws JsonLdError {
                    return null;
                }
                
                @Override
                public Optional<JsonStructure> getJsonStructure() throws JsonLdError {
                    return null;
                }
            }));
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }    
}
