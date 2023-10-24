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
package no.hasmac.jsonld.http;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import no.hasmac.jsonld.http.media.MediaType;

class MediaTypeTest {

    @Test
    void testNull() {
        assertThrows(IllegalArgumentException.class, () -> MediaType.of(null));
    }

    @Test
    void test1() {
        MediaType type = MediaType.of("text/plain");
        assertNotNull(type);
        assertEquals("text", type.type());
        assertEquals("plain", type.subtype());
        assertEquals("text/plain", type.toString());
        assertTrue(type.parameters().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"text", "text//xxx", "1/!", "   ", "&"})
    void test2(String mediaType) {
        MediaType type = MediaType.of(mediaType);
        assertNull(type);
    }

    @Test
    void test4() {
        MediaType type = MediaType.of("  x/y+a  ");
        assertNotNull(type);
        assertEquals("x", type.type());
        assertEquals("y+a", type.subtype());
        assertEquals("x/y+a", type.toString());
        assertTrue(type.parameters().isEmpty());
    }

    @Test
    void test6() {
        MediaType type = MediaType.of("1/2.a;3=4;5");
        assertNotNull(type);
        assertEquals("1", type.type());
        assertEquals("2.a", type.subtype());
        assertEquals(new HashSet<>(Arrays.asList("3", "5")), type.parameters().names());
        assertTrue(type.parameters().firstValue("3").isPresent());
        assertEquals("4", type.parameters().firstValue("3").get());
        assertTrue(type.parameters().firstValue("5").isPresent());
        assertEquals("5", type.parameters().firstValue("5").get());
    }

    @Test
    void test7() {
        MediaType type = MediaType.of("A#/Z^ ; a = \"4\"");
        assertNotNull(type);
        assertEquals("A#", type.type());
        assertEquals("Z^", type.subtype());
        assertEquals(new HashSet<>(Arrays.asList("a")), type.parameters().names());
        assertTrue(type.parameters().firstValue("a").isPresent());
        assertEquals("4", type.parameters().firstValue("a").get());
        assertEquals(Arrays.asList("4"), type.parameters().values("a"));
        assertEquals(Collections.emptyList(), type.parameters().values("Z"));
    }

    @Test
    void test8() {
        MediaType type = MediaType.of("a/b;1;2=3");
        assertNotNull(type);
        assertEquals("a", type.type());
        assertEquals("b", type.subtype());
        assertEquals(new HashSet<>(Arrays.asList("1", "2")), type.parameters().names());
        assertTrue(type.parameters().firstValue("1").isPresent());
        assertEquals("1", type.parameters().firstValue("1").get());
        assertTrue(type.parameters().firstValue("2").isPresent());
        assertEquals("3", type.parameters().firstValue("2").get());
    }

    @Test
    void test10() {
        MediaType type= MediaType.of("a/b !");
        assertNotNull(type);
        assertEquals("a", type.type());
        assertEquals("b", type.subtype());
        assertTrue(type.parameters().isEmpty());
        assertEquals(Collections.emptySet(), type.parameters().names());
    }

    @Test
    void test11() {
        MediaType type= MediaType.of("a/b;1= 2; 1=\"3\"");
        assertNotNull(type);
        assertEquals("a", type.type());
        assertEquals("b", type.subtype());
        assertEquals(new HashSet<>(Arrays.asList("1")), type.parameters().names());
        assertTrue(type.parameters().firstValue("1").isPresent());
        assertEquals("2", type.parameters().firstValue("1").get());
        assertEquals(Arrays.asList("2", "3"), type.parameters().values("1"));
    }

    @Test
    void test12() {
        MediaType type= MediaType.of("a/b;1=\"a\\\tb\"");
        assertNotNull(type);
        assertEquals("a", type.type());
        assertEquals("b", type.subtype());
        assertEquals(new HashSet<>(Arrays.asList("1")), type.parameters().names());
        assertTrue(type.parameters().firstValue("1").isPresent());
        assertEquals("a\tb", type.parameters().firstValue("1").get());
        assertEquals(Arrays.asList("a\tb"), type.parameters().values("1"));
    }

    @Test
    void test14() {
        assertThrows(IllegalArgumentException.class, () -> MediaType.of(null, "a"));
    }

    @Test
    void test15() {
        assertThrows(IllegalArgumentException.class, () -> MediaType.of("b", null));
    }

    @Test
    void testM01() {
        assertTrue(MediaType.ANY.match(MediaType.HTML));
    }

    @Test
    void testM02() {
        assertTrue(MediaType.JSON_LD.match(MediaType.JSON_LD));
    }

    @Test
    void testM03() {
        assertFalse(MediaType.JSON.match(MediaType.JSON_LD));
    }
}
