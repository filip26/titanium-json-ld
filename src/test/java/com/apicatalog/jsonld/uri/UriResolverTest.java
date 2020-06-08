package com.apicatalog.jsonld.uri;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.uri.UriResolver;

@RunWith(Parameterized.class)
public class UriResolverTest {

    @Parameterized.Parameter(0)
    public URI base;

    @Parameterized.Parameter(1)
    public String relative;

    @Parameterized.Parameter(2)
    public String expected;

    @Test
    public void testResolve() {
        Assert.assertEquals(expected, UriResolver.resolve(base, relative));
    }
    
    @Parameterized.Parameters(name = "resolve({0}, {1}) to {2}")
    public static Collection<Object[]> data() {        
        List<Object[]> data = new ArrayList<>();
        
        data.add(new Object[] {URI.create("file:///a/bb/ccc/d;p?q,"), "g", "file:///a/bb/ccc/g"});
        data.add(new Object[] {URI.create("file:///a/bb/ccc/d;p?q,"), "/g", "file:///g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g:h", "g:h"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g", "http://a/b/c/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "./g", "http://a/b/c/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g/", "http://a/b/c/g/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "/g", "http://a/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "//g", "http://g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "?y", "http://a/b/c/d;p?y"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g?y", "http://a/b/c/g?y"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "#s", "http://a/b/c/d;p?q#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g#s", "http://a/b/c/g#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g?y#s", "http://a/b/c/g?y#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), ";x", "http://a/b/c/;x"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g;x", "http://a/b/c/g;x"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "g;x?y#s", "http://a/b/c/g;x?y#s"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "", "http://a/b/c/d;p?q"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), ".", "http://a/b/c/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "./", "http://a/b/c/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "..", "http://a/b/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../", "http://a/b/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../g", "http://a/b/g"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../..", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c/d;p?q"), "../../g", "http://a/g"});
        data.add(new Object[] {URI.create("http://a/"), "", "http://a/"});
        data.add(new Object[] {URI.create("http://a/b/c"), "/b", "http://a/b"});
        
        return data;

    }
}
