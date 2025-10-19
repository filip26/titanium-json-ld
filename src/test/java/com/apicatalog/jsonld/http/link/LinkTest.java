/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.http.link;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.http.media.MediaType;

class LinkTest {

    @Test
    void testNullNull() {
        assertThrows(NullPointerException.class, () -> Link.of(null, null));
    }

    @Test
    void testEmptyNull() {
        Collection<Link> result = Link.of("", null);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void test1() {
        Collection<Link> result = Link.of("<http://example.com/TheBook/chapter2>; rel=\"previous\"\t; title=\"previous chapter\"");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertEquals(URI.create("http://example.com/TheBook/chapter2"), l1.target());
        assertEquals(new HashSet<>(Arrays.asList("title")), l1.attributeNames());

        assertEquals(Arrays.asList(new LinkAttribute("title", "previous chapter")), l1.attributes("title"));

        assertEquals(new HashSet<>(Arrays.asList("previous")), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test2() {
        Collection<Link> result = Link.of("</>; rel=\"http://example.net/foo\"");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertEquals(URI.create("/"), l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());

        assertEquals(new HashSet<>(Arrays.asList("http://example.net/foo")), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test3() {
        Collection<Link> result = Link.of("</terms> ;rel=\"copyright\"; anchor=\"#foo\"");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertEquals(URI.create("/terms"), l1.target());
        assertEquals(URI.create("#foo"), l1.context());
        assertEquals(Collections.emptySet(), l1.attributeNames());

        assertEquals(new HashSet<>(Arrays.asList("copyright")), l1.relations());

        assertNull(l1.type());
    }

    @Test
    void test4() {
        Collection<Link> result = Link.of("    <http://example.org/> ; rel =  \" start     http://example.net/relation/other\" ");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertEquals(URI.create("http://example.org/"), l1.target());
        assertTrue(l1.attributeMap().isEmpty());
        assertEquals(Collections.emptySet(), l1.attributeNames());

        assertEquals(new HashSet<>(Arrays.asList("start", "http://example.net/relation/other")), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test5() {
        Collection<Link> result = Link.of("<https://example.org/>;rel=\"st\\\\art\",<https://example.org/index>;rel=\"i\\αn\\tdex\"");
        assertNotNull(result);
        assertEquals(2, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("https://example.org/"), l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());
        assertTrue(l1.attributeMap().isEmpty());

        assertEquals(new HashSet<>(Arrays.asList("st\\art")), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());

        Link l2 = it.next();

        assertEquals(URI.create("https://example.org/index"), l2.target());
        assertTrue(l2.attributeMap().isEmpty());
        assertEquals(Collections.emptySet(), l2.attributeNames());

        assertEquals(new HashSet<>(Arrays.asList("iαn", "dex")), l2.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test6() {
        Collection<Link> result = Link.of("<>;rel=123 ");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertNull(l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());

        assertEquals(new HashSet<>(Arrays.asList("123")), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test7() {
        Collection<Link> result = Link.of("<>;x=1x10,</>");
        assertNotNull(result);
        assertEquals(2, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertNull(l1.target());
        assertEquals(new HashSet<>(Arrays.asList("x")), l1.attributeNames());

        assertEquals("x=1x10", l1.findFirstAttribute("x").map(Object::toString).orElse(null));

        assertNull(l1.type());
        assertNull(l1.context());

        Link l2 = it.next();

        assertEquals(URI.create("/"), l2.target());

        assertEquals(Collections.emptySet(), l2.attributeNames());

        assertEquals(Collections.emptySet(), l2.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test8() {
        Collection<Link> result = Link.of("<x>;x\t=ab;");
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("x"), l1.target());
        assertEquals(new HashSet<>(Arrays.asList("x")), l1.attributeNames());

        Optional<LinkAttribute> value = l1.findFirstAttribute("x");
        assertTrue(value.isPresent());

        assertEquals("x=ab", value.get().toString());
        assertEquals("x", value.get().name());
        assertEquals("ab", value.get().value());
        assertNull(value.get().languageTag());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test9() {
        Collection<Link> result = Link.of("</>;type=\"text/html\"");
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("/"), l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());
        assertEquals(Collections.emptySet(), l1.relations());
        assertTrue(MediaType.HTML.match(l1.type()));
        assertNull(l1.context());
    }

    @Test
    void test10() {
        Collection<Link> result = Link.of("</x>;", URI.create("https://a/b/c"));
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("https://a/x"), l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());
        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test11() {
        Collection<Link> result = Link.of("<x>;abc,", URI.create("https://a/b/c"));
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("https://a/b/x"), l1.target());
        assertEquals(new HashSet<>(Arrays.asList("abc")), l1.attributeNames());
        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test12() {
        Collection<Link> result = Link.of("<x>;anchor=\"/anchor\"", URI.create("//a/b/c"));
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("//a/b/x"), l1.target());
        assertEquals(Collections.emptySet(), l1.attributeNames());
        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertEquals(URI.create("//a/anchor"), l1.context());
    }

    @Test
    void test13() {
        Collection<Link> result = Link.of(" \t<#abc>  ;   x  =\t 123 \t;  x   ;y   , \t\t ");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertEquals(URI.create("#abc"), l1.target());
        assertEquals(new HashSet<>(Arrays.asList("x", "y")), l1.attributeNames());

        assertTrue(l1.findFirstAttribute("x").isPresent());
        assertEquals("123", l1.findFirstAttribute("x").get().value());

        assertEquals(Arrays.asList(new LinkAttribute("x", "123"), new LinkAttribute("x", "x")), l1.attributes("x"));
        assertEquals(Arrays.asList(new LinkAttribute("y", "y")), l1.attributes("y"));

        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test14() {
        Collection<Link> result = Link.of("<>;1=2");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertNull(l1.target());

        assertEquals(Arrays.asList(new LinkAttribute("1", "2")), l1.attributes());

        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void test15() {
        Collection<Link> result = Link.of("<>;1=23");
        assertNotNull(result);
        assertEquals(1, result.size());

        Link l1 = result.iterator().next();

        assertNull(l1.target());

        assertEquals(Arrays.asList(new LinkAttribute("1", "23")), l1.attributes());

        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void testI1() {
        Collection<Link> result = Link.of("<https://example.org/");
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void testI2() {
        Collection<Link> result = Link.of("https://example.org/");
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void testI3() {
        Collection<Link> result = Link.of("<https://example.org/> #");
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("https://example.org/"), l1.target());

        assertTrue(l1.attributeMap().isEmpty());
        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void testI4() {
        Collection<Link> result = Link.of("<https://example.org/>   ; x;y");
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertEquals(URI.create("https://example.org/"), l1.target());

        assertEquals(new HashSet<>(Arrays.asList("x", "y")), l1.attributeNames());
        assertFalse(l1.attributeMap().isEmpty());

        assertEquals(Arrays.asList(new LinkAttribute("x"), new LinkAttribute("y")), l1.attributes());

        assertTrue(l1.findFirstAttribute("x").isPresent());
        assertTrue(l1.findFirstAttribute("y").isPresent());

        assertEquals("x", l1.findFirstAttribute("x").get().value());
        assertEquals("y", l1.findFirstAttribute("y").get().value());

        assertEquals(Arrays.asList(new LinkAttribute("x")), l1.attributes("x"));
        assertEquals(Arrays.asList(new LinkAttribute("y")), l1.attributes("y"));

        assertEquals(Collections.emptySet(), l1.relations());

        assertNull(l1.type());
        assertNull(l1.context());
    }

    @Test
    void testI5() {
        Collection<Link> result = Link.of("<>;type=\"text\"");
        assertNotNull(result);
        assertEquals(1, result.size());

        Iterator<Link> it = result.iterator();

        Link l1 = it.next();

        assertNull(l1.target());
        assertEquals(new HashSet<>(Arrays.asList("type")), l1.attributeNames());
        assertTrue(l1.findFirstAttribute("type").isPresent());
        assertEquals("text", l1.findFirstAttribute("type").get().value());

        assertEquals(Collections.emptySet(), l1.relations());
        assertNull(l1.type());
        assertNull(l1.context());
    }
}
