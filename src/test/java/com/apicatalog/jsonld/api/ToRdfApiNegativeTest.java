package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ToRdfApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.toRdf((RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.toRdf((String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.toRdf((URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.toRdf(URI.create("/relative"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.toRdf("");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.toRdf("   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.toRdf("relative");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        JsonLd.toRdf(new RemoteDocument(null));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test9() {
        JsonLd.toRdf(new RemoteDocument(new RemoteContent() {
            
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
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.toRdf("http://example.org").base("!//");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.toRdf("http://example.org").context("~");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.toRdf("http://example.org").options(null);
    }
}
