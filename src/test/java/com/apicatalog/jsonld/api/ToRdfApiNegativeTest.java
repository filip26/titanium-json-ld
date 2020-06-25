package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;

public class ToRdfApiNegativeTest {

    @Test    
    public void test1() {
        try {
            JsonLd.toRdf((RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test2() {
        try {
            JsonLd.toRdf((String)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test3() {
        try {
            JsonLd.toRdf((URI)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test4() {
        try {
            JsonLd.toRdf(URI.create("/relative"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test5() {
        try {
            JsonLd.toRdf("");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test6() {
        try {
            JsonLd.toRdf("   ");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test7() {
        try {
            JsonLd.toRdf("relative");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test8() {
        try {
            JsonLd.toRdf(new RemoteDocument(null));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test9() {
        try {
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
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }   
    
    
    @Test
    public void test10() {
        try {
            JsonLd.toRdf("http://example.org").base("!//");
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test11() {
        try {
            JsonLd.toRdf("http://example.org").context("~");
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test12() {
        try {
            JsonLd.toRdf("http://example.org").options(null);
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
}
