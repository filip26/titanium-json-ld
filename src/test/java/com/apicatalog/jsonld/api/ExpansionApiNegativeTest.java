package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ExpansionApiNegativeTest {

    @Test    
    public void test1() {
        try {
            JsonLd.expand((RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test2() {
        try {
            JsonLd.expand((String)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test3() {
        try {
            JsonLd.expand((URI)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test4() {
        try {
            JsonLd.expand("");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test5() {
        try {
            JsonLd.expand("   ");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test6() {
        try {
            JsonLd.expand("/relative");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test7() {
        try {
            JsonLd.expand(URI.create("relative"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test8() {
        try {
            JsonLd.expand(new RemoteDocument(new Document() {
                
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

    @Test
    public void test9() {
        try {
            JsonLd.expand(new RemoteDocument());
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }    
    
    @Test
    public void test10() {
        try {
            JsonLd.expand("http://example.org").base("!//");
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test11() {
        try {
            JsonLd.expand("http://example.org").context("~");
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test12() {
        try {
            JsonLd.expand("http://example.org").options(null);
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
}
