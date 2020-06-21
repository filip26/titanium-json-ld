package com.apicatalog.jsonld.http.link;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.http.MediaType;

public class LinkTest {

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
        
        Assert.assertEquals(URI.create("http://example.com/TheBook/chapter2"), l1.target());
        Assert.assertEquals(new HashSet<>(Arrays.asList("title")), l1.attributes().names());
        
        Assert.assertEquals(Arrays.asList(new LinkAttribute("title", "previous chapter")), l1.attributes().values("title"));
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("previous")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test2() {
        Collection<Link> result = Link.valueOf("</>; rel=\"http://example.net/foo\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("/"), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("http://example.net/foo")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }
    
    @Test
    public void test3() {
        Collection<Link> result = Link.valueOf("</terms> ;rel=\"copyright\"; anchor=\"#foo\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("/terms"), l1.target());
        Assert.assertTrue(l1.context().isPresent());
        Assert.assertEquals(URI.create("#foo"), l1.context().get());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
   
        Assert.assertEquals(new HashSet<>(Arrays.asList("copyright")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());
    }
    
    @Test
    public void test4() {
        Collection<Link> result = Link.valueOf("    <http://example.org/> ; rel=\" start     http://example.net/relation/other\" ");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create("http://example.org/"), l1.target());
        Assert.assertTrue(l1.attributes().isEmpty());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("start", "http://example.net/relation/other")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test5() {
        Collection<Link> result = Link.valueOf("<https://example.org/>;rel=\"st\\\\art\",<https://example.org/index>;rel=\"in\\tdex\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(2, result.size());
        
        Iterator<Link> it = result.iterator();
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        Assert.assertTrue(l1.attributes().isEmpty());
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("st\\art")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
        
        Link l2 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/index"), l2.target());
        Assert.assertTrue(l2.attributes().isEmpty());
        Assert.assertEquals(Collections.emptySet(), l2.attributes().names());
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("in", "dex")), l2.relations());
        
        Assert.assertTrue(l2.type().isEmpty());        
        Assert.assertTrue(l2.context().isEmpty());
    }

    @Test
    public void test6() {
        Collection<Link> result = Link.valueOf("<>;rel=123");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Link l1 = result.iterator().next();
        
        Assert.assertEquals(URI.create(""), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        
        Assert.assertEquals(new HashSet<>(Arrays.asList("123")), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test7() {
        Collection<Link> result = Link.valueOf("<>;x=1x10,</>");
        Assert.assertNotNull(result);
        Assert.assertEquals(2, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create(""), l1.target());
        Assert.assertEquals(new HashSet<>(Arrays.asList("x")), l1.attributes().names());
        
        Assert.assertEquals("x=1x10", l1.attributes().firstValue("x").map(Object::toString).orElse(null));
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
        
        Link l2 = it.next();
        
        Assert.assertEquals(URI.create("/"), l2.target());

        Assert.assertEquals(Collections.emptySet(), l2.attributes().names());
        
        Assert.assertEquals(Collections.emptySet(), l2.relations());
        
        Assert.assertTrue(l2.type().isEmpty());        
        Assert.assertTrue(l2.context().isEmpty());
    }

    @Test
    public void test8() {
        Collection<Link> result = Link.valueOf("<x>;x=ab;");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("x"), l1.target());
        Assert.assertEquals(new HashSet<>(Arrays.asList("x")), l1.attributes().names());
        
        Optional<LinkAttribute> value = l1.attributes().firstValue("x");
        Assert.assertTrue(value.isPresent());
                
        Assert.assertEquals("x=ab", value.get().toString());
        Assert.assertEquals("x", value.get().name());
        Assert.assertEquals("ab", value.get().value());
        Assert.assertTrue(value.get().languageTag().isEmpty());
        
        Assert.assertTrue(l1.type().isEmpty());
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test9() {
        Collection<Link> result = Link.valueOf("</>;type=\"text/html\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("/"), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        Assert.assertTrue(l1.type().isPresent());
        Assert.assertTrue(MediaType.HTML.match(l1.type().get()));
        Assert.assertTrue(l1.context().isEmpty());
    }
    
    @Test
    public void test10() {
        Collection<Link> result = Link.valueOf("</x>;", URI.create("https://a/b/c"));
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://a/x"), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        Assert.assertTrue(l1.type().isEmpty());
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test11() {
        Collection<Link> result = Link.valueOf("<x>;abc,", URI.create("https://a/b/c"));
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://a/b/x"), l1.target());
        Assert.assertEquals(new HashSet<>(Arrays.asList("abc")), l1.attributes().names());
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        Assert.assertTrue(l1.type().isEmpty());
        Assert.assertTrue(l1.context().isEmpty());
    }

    @Test
    public void test12() {
        Collection<Link> result = Link.valueOf("<x>;anchor=\"/anchor\"", URI.create("//a/b/c"));
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("//a/b/x"), l1.target());
        Assert.assertEquals(Collections.emptySet(), l1.attributes().names());
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        Assert.assertTrue(l1.type().isEmpty());
        Assert.assertTrue(l1.context().isPresent());
        Assert.assertEquals(URI.create("//a/anchor"), l1.context().get());
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
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.target());
        
        Assert.assertTrue(l1.attributes().isEmpty());
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }
    
    @Test
    public void testI4() {
        Collection<Link> result = Link.valueOf("<https://example.org/>   ; x;y");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        
        Iterator<Link> it = result.iterator();
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create("https://example.org/"), l1.target());

        Assert.assertEquals(new HashSet<>(Arrays.asList("x", "y")), l1.attributes().names());
        Assert.assertFalse(l1.attributes().isEmpty());
        
        Assert.assertEquals(Arrays.asList(new LinkAttribute("x"), new LinkAttribute("y")), l1.attributes().values());
        
        Assert.assertTrue(l1.attributes().firstValue("x").isPresent());
        Assert.assertTrue(l1.attributes().firstValue("y").isPresent());
        
        Assert.assertEquals("x", l1.attributes().firstValue("x").get().value());
        Assert.assertEquals("y", l1.attributes().firstValue("y").get().value());
        
        Assert.assertEquals(Arrays.asList(new LinkAttribute("x")), l1.attributes().values("x"));
        Assert.assertEquals(Arrays.asList(new LinkAttribute("y")), l1.attributes().values("y"));
        
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        
        Assert.assertTrue(l1.type().isEmpty());        
        Assert.assertTrue(l1.context().isEmpty());
    }
    
    @Test
    public void testI5() {
        Collection<Link> result = Link.valueOf("<>;type=\"text\"");
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());

        Iterator<Link> it = result.iterator(); 
        
        Link l1 = it.next();
        
        Assert.assertEquals(URI.create(""), l1.target());
        Assert.assertEquals(new HashSet<>(Arrays.asList("type")), l1.attributes().names());
        Assert.assertTrue(l1.attributes().firstValue("type").isPresent());
        Assert.assertEquals("text", l1.attributes().firstValue("type").get().value());
        
        Assert.assertEquals(Collections.emptySet(), l1.relations());
        Assert.assertTrue(l1.type().isEmpty());
        Assert.assertTrue(l1.context().isEmpty());
    }


}
