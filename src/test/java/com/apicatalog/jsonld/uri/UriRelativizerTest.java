package com.apicatalog.jsonld.uri;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class UriRelativizerTest {

    @Parameterized.Parameter(0)
    public URI base;

    @Parameterized.Parameter(2)
    public String uri;

    @Parameterized.Parameter(1)
    public String expected;

    @Test
    public void testRelativize() {
        Assert.assertEquals(expected, UriRelativizer.relativize(base, uri));
    }

    
    @Parameterized.Parameters(name = "relativize({0}, {2}) to {1}")
    public static Collection<Object[]> data() {
        List<Object[]> data = new ArrayList<>();
        
    
        data.add(new Object[] {URI.create("http://example.com/"), "person/1", "http://example.com/person/1"});
        data.add(new Object[] {URI.create("http://example.com"), "/person/1", "http://example.com/person/1"});
        data.add(new Object[] {URI.create("https://example.com/"), "relative-url", "https://example.com/relative-url"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "link", "https://w3c.github.io/json-ld-api/tests/compact/link"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "#fragment-works", "https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld#fragment-works"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "?query=works", "https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld?query=works"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../", "https://w3c.github.io/json-ld-api/tests/"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../", "https://w3c.github.io/json-ld-api/"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../parent", "https://w3c.github.io/json-ld-api/parent"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../parent#fragment", "https://w3c.github.io/json-ld-api/parent#fragment"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../../parent-parent-eq-root", "https://w3c.github.io/parent-parent-eq-root"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../../still-root", "https://w3c.github.io/still-root"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../../too-many-dots", "https://w3c.github.io/too-many-dots"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "../../../absolute", "https://w3c.github.io/absolute"});
        data.add(new Object[] {URI.create("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld"), "http://example.org/scheme-relative", "http://example.org/scheme-relative"});
        
        data.add(new Object[] {URI.create("http://a"), "/b", "http://a/b"});
        data.add(new Object[] {URI.create("http://a/"), "./", "http://a/"});
        data.add(new Object[] {URI.create("http://a/1/2"), "2", "http://a/1/2"});
        data.add(new Object[] {URI.create("http://a/1/2/"), "./", "http://a/1/2/"});
        data.add(new Object[] {URI.create("http://a/1/2"), "./", "http://a/1/2/"});
        data.add(new Object[] {URI.create("http://a"), "/", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g:h", "g:h"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g", "http://a/b/c/g"});
        data.add(new Object[] {URI.create("http://a/b/c/?q"), "g", "http://a/b/c/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g/", "http://a/b/c/g/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../g", "http://a/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "?y", "http://a/b/c/d;p?y"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g?y", "http://a/b/c/g?y"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "#s", "http://a/b/c/d;p?q#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g#s", "http://a/b/c/g#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g?y#s", "http://a/b/c/g?y#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), ";x", "http://a/b/c/;x"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g;x", "http://a/b/c/g;x"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g;x?y#s", "http://a/b/c/g;x?y#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "d;p", "http://a/b/c/d;p?q"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../", "http://a/b/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../", "http://a/b/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../g", "http://a/b/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../g", "http://a/g"});
        
        return data;

    }

    
}
