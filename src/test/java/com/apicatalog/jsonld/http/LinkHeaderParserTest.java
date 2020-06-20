package com.apicatalog.jsonld.http;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;

import org.junit.Assert;
import org.junit.Test;

public class LinkHeaderParserTest {

    @Test
    public void testNullNull() {
        try {
        
            Link.valueOf(null, null);
            Assert.fail();
            
        } catch (IllegalArgumentException e) {
            return;
        }
    }

    @Test
    public void testEmptyNull() {
        Collection<Link> result = Link.valueOf("", null);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void test1() {
        Collection<Link> result = Link.valueOf("<http://example.com/TheBook/chapter2>; rel=\"previous\"; title=\"previous chapter\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("http://example.com/TheBook/chapter2"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel", "title")), l1.paramNames());
        
        Assert.assertEquals("previous", l1.paramValue("rel"));
        Assert.assertEquals("previous chapter", l1.paramValue("title"));
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("previous")), l1.rel());
        
        Assert.assertNull(l1.type());
    }

    @Test
    public void test2() {
        Collection<Link> result = Link.valueOf("</>; rel=\"http://example.net/foo\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("/"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel")), l1.paramNames());
        
        Assert.assertEquals("http://example.net/foo", l1.paramValue("rel"));        
        Assert.assertEquals(new HashSet<>(Arrays.asList("http://example.net/foo")), l1.rel());
        
        Assert.assertNull(l1.type());
    }
    
    @Test
    public void test3() {
        Collection<Link> result = Link.valueOf("</terms> ;rel=\"copyright\"; anchor=\"#foo\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("/terms"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel", "anchor")), l1.paramNames());
        
        Assert.assertEquals("copyright", l1.paramValue("rel"));
        Assert.assertEquals("#foo", l1.paramValue("anchor"));   
        Assert.assertEquals(new HashSet<>(Arrays.asList("copyright")), l1.rel());
        
        Assert.assertNull(l1.type());
    }
    
    @Test
    public void test4() {
        Collection<Link> result = Link.valueOf("    <http://example.org/> ; rel=\" start     http://example.net/relation/other\" ");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("http://example.org/"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel")), l1.paramNames());
        
        Assert.assertEquals("start     http://example.net/relation/other", l1.paramValue("rel"));
        Assert.assertEquals(new HashSet<>(Arrays.asList("start", "http://example.net/relation/other")), l1.rel());
        
        Assert.assertNull(l1.type());
    }

    @Test
    public void test5() {
        Collection<Link> result = Link.valueOf("<https://example.org/>;rel=\"st\\\\art\",<https://example.org/index>;rel=\"in\\tdex\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(2, result.size());
        
        Iterator<Link> it = result.iterator();
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel")), l1.paramNames());
        
        Assert.assertEquals("st\\art", l1.paramValue("rel"));
        Assert.assertEquals(new HashSet<>(Arrays.asList("st\\art")), l1.rel());
        Assert.assertNull(l1.type());
        
        Link l2 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/index"), l2.uri());
        Assert.assertNotNull(l2.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("rel")), l2.paramNames());
        
        Assert.assertEquals("intdex", l2.paramValue("rel"));
        Assert.assertEquals(new HashSet<>(Arrays.asList("intdex")), l2.rel());
        Assert.assertNull(l2.type());
    }
    
    @Test
    public void testI1() {
        Collection<Link> result = Link.valueOf("<https://example.org/");
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());        
    }
    
    @Test
    public void testI2() {
        Collection<Link> result = Link.valueOf("https://example.org/");
        Assert.assertNotNull(result);
        Assert.assertEquals(0, result.size());        
    }
    
    @Test
    public void testI3() {
        Collection<Link> result = Link.valueOf("<https://example.org/> #");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Iterator<Link> it = result.iterator();
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(Collections.emptySet(), l1.paramNames());
        Assert.assertNull(l1.type());        
        
    }
    
    @Test
    public void testI4() {
        Collection<Link> result = Link.valueOf("<https://example.org/>   ; x;y");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Iterator<Link> it = result.iterator();
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.uri());
        Assert.assertNotNull(l1.paramNames());
        Assert.assertEquals(new HashSet<>(Arrays.asList("x", "y")), l1.paramNames());

        Assert.assertNull(l1.paramValue("x"));
        Assert.assertNull(l1.paramValue("y"));
        Assert.assertEquals(Collections.emptySet(), l1.rel());
        Assert.assertNull(l1.type());        
    }
}
