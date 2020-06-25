package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;

public class FramingApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.frame((RemoteDocument)null, (RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.frame(RemoteDocument.of(Json.createArrayBuilder().build()), (RemoteDocument)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.frame((RemoteDocument)null, RemoteDocument.of(Json.createArrayBuilder().build()));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.frame((String)null, (String)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.frame("https://example.org", null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.frame(null, "http://example.com");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.frame("", "http://example.com");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        JsonLd.frame("http://example.org/", "");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test9() {
        JsonLd.frame("http://example.org", "   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.frame("http://example.org", "relative");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.frame("relative", "http://example.org");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.frame((URI)null, (URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test13() {
        JsonLd.frame(URI.create("http://example.org"), null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test14() {
        JsonLd.frame(null, URI.create("http://example.org"));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test15() {
        JsonLd.frame(URI.create("/relative"), URI.create("http://example.com"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test16() {
        JsonLd.frame(new RemoteDocument(new RemoteContent() {
            
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
    }

    @Test(expected = IllegalArgumentException.class)
    public void test17() {
        JsonLd.frame(
            RemoteDocument.of(Json.createArrayBuilder().build()),
            new RemoteDocument(new RemoteContent() {
            
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
    }    
}
