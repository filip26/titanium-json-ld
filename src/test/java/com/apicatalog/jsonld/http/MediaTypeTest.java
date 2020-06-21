package com.apicatalog.jsonld.http;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.http.media.MediaType;

public class MediaTypeTest {


    @Test
    public void testNull() {
        try {
            MediaType.of(null);
            Assert.fail();
            
        } catch (IllegalArgumentException e) {

        }
    }
    
    @Test
    public void test1() {
        MediaType type = MediaType.of("text/plain");
        Assert.assertNotNull(type);
        Assert.assertEquals("text", type.type());
        Assert.assertEquals("plain", type.subtype());
        Assert.assertEquals("text/plain", type.toString());
        Assert.assertTrue(type.parameters().isEmpty());
    }

    @Test
    public void test2() {
        MediaType type = MediaType.of("text");
        Assert.assertNull(type);
    }

    @Test
    public void test3() {
        MediaType type = MediaType.of("text//xxx");
        Assert.assertNull(type);
    }

    @Test
    public void test4() {
        MediaType type = MediaType.of("  x/y+a  ");
        Assert.assertNotNull(type);
        Assert.assertEquals("x", type.type());
        Assert.assertEquals("y+a", type.subtype());
        Assert.assertEquals("x/y+a", type.toString());
        Assert.assertTrue(type.parameters().isEmpty());
    }

    @Test
    public void test5() {
        MediaType type = MediaType.of("1/!");
        Assert.assertNull(type);
    }
    
    @Test
    public void test6() {
        MediaType type = MediaType.of("1/2.a;3=4;5");
        Assert.assertNotNull(type);
        Assert.assertEquals("1", type.type());
        Assert.assertEquals("2.a", type.subtype());
        Assert.assertEquals(new HashSet<>(Arrays.asList("3", "5")), type.parameters().names());
        Assert.assertTrue(type.parameters().firstValue("3").isPresent());
        Assert.assertEquals("4", type.parameters().firstValue("3").get());
        Assert.assertTrue(type.parameters().firstValue("5").isPresent());
        Assert.assertEquals("5", type.parameters().firstValue("5").get());
    }

    @Test
    public void test7() {
        MediaType type = MediaType.of("A#/Z^ ; a = \"4\"");
        Assert.assertNotNull(type);
        Assert.assertEquals("A#", type.type());
        Assert.assertEquals("Z^", type.subtype());
        Assert.assertEquals(new HashSet<>(Arrays.asList("a")), type.parameters().names());
        Assert.assertTrue(type.parameters().firstValue("a").isPresent());
        Assert.assertEquals("4", type.parameters().firstValue("a").get());
        Assert.assertEquals(Arrays.asList("4"), type.parameters().values("a"));
        Assert.assertEquals(Collections.emptyList(), type.parameters().values("Z"));
    }

    @Test
    public void test8() {
        MediaType type = MediaType.of("a/b;1;2=3");
        Assert.assertNotNull(type);
        Assert.assertEquals("a", type.type());
        Assert.assertEquals("b", type.subtype());
        Assert.assertEquals(new HashSet<>(Arrays.asList("1", "2")), type.parameters().names());
        Assert.assertTrue(type.parameters().firstValue("1").isPresent());
        Assert.assertEquals("1", type.parameters().firstValue("1").get());
        Assert.assertTrue(type.parameters().firstValue("2").isPresent());
        Assert.assertEquals("3", type.parameters().firstValue("2").get());
    }

    @Test
    public void test9() {
        MediaType type = MediaType.of("   ");
        Assert.assertNull(type);
    }

    @Test
    public void test10() {
        MediaType type= MediaType.of("a/b !");
        Assert.assertNotNull(type);
        Assert.assertEquals("a", type.type());
        Assert.assertEquals("b", type.subtype());
        Assert.assertTrue(type.parameters().isEmpty());
        Assert.assertEquals(Collections.emptySet(), type.parameters().names());
    }

    @Test
    public void test11() {
        MediaType type= MediaType.of("a/b;1= 2; 1=\"3\"");
        Assert.assertNotNull(type);
        Assert.assertEquals("a", type.type());
        Assert.assertEquals("b", type.subtype());
        Assert.assertEquals(new HashSet<>(Arrays.asList("1")), type.parameters().names());
        Assert.assertTrue(type.parameters().firstValue("1").isPresent());
        Assert.assertEquals("2", type.parameters().firstValue("1").get());
        Assert.assertEquals(Arrays.asList("2", "3"), type.parameters().values("1"));
    }
    
    @Test
    public void test12() {
        MediaType type= MediaType.of("a/b;1=\"a\\\tb\"");
        Assert.assertNotNull(type);
        Assert.assertEquals("a", type.type());
        Assert.assertEquals("b", type.subtype());
        Assert.assertEquals(new HashSet<>(Arrays.asList("1")), type.parameters().names());
        Assert.assertTrue(type.parameters().firstValue("1").isPresent());
        Assert.assertEquals("a\tb", type.parameters().firstValue("1").get());
        Assert.assertEquals(Arrays.asList("a\tb"), type.parameters().values("1"));
    }

    @Test
    public void test13() {
        MediaType type= MediaType.of("&");
        Assert.assertNull(type);
    }

    @Test
    public void test14() {
        try {
            MediaType.of(null, "a");
            Assert.fail();
            
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void test15() {
        try {
            MediaType.of("b", null);
            Assert.fail();
            
        } catch (IllegalArgumentException e) {

        }
    }
    @Test
    public void testM01() {
        Assert.assertTrue(MediaType.ANY.match(MediaType.HTML));
    }

    @Test
    public void testM02() {
        Assert.assertTrue(MediaType.JSON_LD.match(MediaType.JSON_LD));
    }

    @Test
    public void testM03() {
        Assert.assertFalse(MediaType.JSON.match(MediaType.JSON_LD));
    }

}
