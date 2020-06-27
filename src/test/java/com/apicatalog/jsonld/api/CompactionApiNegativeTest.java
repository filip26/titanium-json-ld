package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.Json;
import javax.json.JsonStructure;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RemoteDocument;

public class CompactionApiNegativeTest {

    @Test(expected = IllegalArgumentException.class)
    public void test1() {
        JsonLd.compact((RemoteDocument)null, (JsonStructure)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test2() {
        JsonLd.compact(RemoteDocument.of(Json.createArrayBuilder().build()), (JsonStructure)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test3() {
        JsonLd.compact((RemoteDocument)null, Json.createArrayBuilder().build());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test4() {
        JsonLd.compact((RemoteDocument)null, (RemoteDocument)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test5() {
        JsonLd.compact(RemoteDocument.of(Json.createArrayBuilder().build()), (RemoteDocument)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        JsonLd.compact((RemoteDocument)null, RemoteDocument.of(Json.createArrayBuilder().build()));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test7() {
        JsonLd.compact((String)null, (String)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        JsonLd.compact("http://example.org/", (String)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test9() {
        JsonLd.compact((String)null, "http://example.org");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        JsonLd.compact("http://example.org", "relative");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test11() {
        JsonLd.compact("relative", "http://example.org");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test12() {
        JsonLd.compact((String)null, Json.createArrayBuilder().build());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test13() {
        JsonLd.compact("http://example.org", (JsonStructure)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test14() {
        JsonLd.compact((String)null, (JsonStructure)null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void test15() {
        JsonLd.compact("/relative", Json.createArrayBuilder().build());
    }
    
    public void test19() {
        JsonLd.compact(new NullRemoteDocument(), RemoteDocument.of(Json.createArrayBuilder().build()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test20() {
        JsonLd.compact((URI)null, (URI)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test21() {
        JsonLd.compact(URI.create("http://example.com"), (URI)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test22() {
        JsonLd.compact((URI)null, URI.create("http://example.com"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test23() {
        JsonLd.compact(URI.create("http://example.com"), URI.create("/relative"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test24() {
        JsonLd.compact(URI.create("/relative"), URI.create("http://example.com"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test25() {
        JsonLd.compact((URI)null, (JsonStructure)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test26() {
        JsonLd.compact(URI.create("http://example.com"), (JsonStructure)null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test27() {
        JsonLd.compact((URI)null, Json.createArrayBuilder().build());
    }

    @Test(expected = IllegalArgumentException.class)
    public void test28() {
        JsonLd.compact(URI.create("relative"), Json.createArrayBuilder().build());
    }

    @Test(expected = IllegalArgumentException.class)
    public void test29() {
        JsonLd.compact("   ", Json.createArrayBuilder().build());
    }

    @Test(expected = IllegalArgumentException.class)
    public void test30() {
        JsonLd.compact("http://example.com", "");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test31() {
        JsonLd.compact("http://example.com", "\t");
    }
}
