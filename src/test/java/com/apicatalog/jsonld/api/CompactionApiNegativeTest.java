package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteContent;
import com.apicatalog.jsonld.document.RemoteDocument;

public class CompactionApiNegativeTest {

    @Test    
    public void test1() {
        try {
            JsonLd.compact((RemoteDocument)null, (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test2() {
        try {
            JsonLd.compact(RemoteDocument.of(Json.createArrayBuilder().build()), (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test3() {
        try {
            JsonLd.compact((RemoteDocument)null, Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test4() {
        try {
            JsonLd.compact((RemoteDocument)null, (RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test5() {
        try {
            JsonLd.compact(RemoteDocument.of(Json.createArrayBuilder().build()), (RemoteDocument)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test6() {
        try {
            JsonLd.compact((RemoteDocument)null, RemoteDocument.of(Json.createArrayBuilder().build()));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test    
    public void test7() {
        try {
            JsonLd.compact((String)null, (String)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test8() {
        try {
            JsonLd.compact("http://example.org/", (String)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test9() {
        try {
            JsonLd.compact((String)null, "http://example.org");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test10() {
        try {
            JsonLd.compact("http://example.org", "relative");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test11() {
        try {
            JsonLd.compact("relative", "http://example.org");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test12() {
        try {
            JsonLd.compact((String)null, Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test13() {
        try {
            JsonLd.compact("http://example.org", (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test14() {
        try {
            JsonLd.compact((String)null, (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    @Test
    public void test15() {
        try {
            JsonLd.compact("/relative", Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test16() {
        try {
            JsonLd.compact(new RemoteDocument(null), Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test17() {
        try {
            JsonLd.compact(new RemoteDocument(new RemoteContent() {
                
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
            }), Json.createArrayBuilder().build());
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test18() {
        try {
            JsonLd.compact(
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
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
    
    public void test19() {
        try {
            JsonLd.compact(new RemoteDocument(new RemoteContent() {
                
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
            }),
            RemoteDocument.of(Json.createArrayBuilder().build()));
            
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test20() {
        try {
            JsonLd.compact((URI)null, (URI)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test21() {
        try {
            JsonLd.compact(URI.create("http://example.com"), (URI)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test22() {
        try {
            JsonLd.compact((URI)null, URI.create("http://example.com"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test23() {
        try {
            JsonLd.compact(URI.create("http://example.com"), URI.create("/relative"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test24() {
        try {
            JsonLd.compact(URI.create("/relative"), URI.create("http://example.com"));
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test25() {
        try {
            JsonLd.compact((URI)null, (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test26() {
        try {
            JsonLd.compact(URI.create("http://example.com"), (JsonStructure)null);
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test27() {
        try {
            JsonLd.compact((URI)null, Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test28() {
        try {
            JsonLd.compact(URI.create("relative"), Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test29() {
        try {
            JsonLd.compact("   ", Json.createArrayBuilder().build());
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test30() {
        try {
            JsonLd.compact("http://example.com", "");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }

    @Test
    public void test31() {
        try {
            JsonLd.compact("http://example.com", "\t");
            Assert.fail();
        
        } catch (IllegalArgumentException e) {}
    }
}
