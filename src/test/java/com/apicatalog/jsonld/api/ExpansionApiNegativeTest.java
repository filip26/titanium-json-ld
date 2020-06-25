package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ExpansionApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.expand((RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.expand((String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.expand((URI)null);
    }
    
    @Test(expected = IllegalArgumentException.class)    
    public void test4() {
        JsonLd.expand("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.expand("   ");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.expand("/relative");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.expand(URI.create("relative"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        JsonLd.expand(new RemoteDocument(new RemoteContent() {
            
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
    public void test9() {
        JsonLd.expand(new RemoteDocument(null));
    }    
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.expand("http://example.org").base("!//");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.expand("http://example.org").context("~");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.expand("http://example.org").options(null);
    }
}
