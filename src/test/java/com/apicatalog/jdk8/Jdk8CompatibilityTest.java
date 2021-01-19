package com.apicatalog.jdk8;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static com.apicatalog.jdk8.Jdk8Compatibility.*;
import static org.junit.jupiter.api.Assertions.*;

class Jdk8CompatibilityTest {

    @Test
    void testStrip() {
        assertNull(strip(null));
        assertEquals("", strip(""));
        assertEquals("", strip("        "));
        assertEquals("abc", strip("  abc  "));
        assertEquals("abc", strip("abc  "));
        assertEquals("abc", strip("  abc"));
        assertEquals("abc", strip("\t\n\rabc\t\r\n"));
    }

    @Test
    void testStripLeading() {
        assertNull(strip(null));
        assertEquals("", stripLeading(""));
        assertEquals("", stripLeading("        "));
        assertEquals("abc  ", stripLeading("  abc  "));
        assertEquals("abc  ", stripLeading("abc  "));
        assertEquals("abc", stripLeading("  abc"));
        assertEquals("abc\t\r\n", stripLeading("\t\n\rabc\t\r\n"));
    }

    @Test
    void testStripTrailing() {
        assertNull(strip(null));
        assertEquals("", stripTrailing(""));
        assertEquals("", stripTrailing("        "));
        assertEquals("  abc", stripTrailing("  abc  "));
        assertEquals("abc", stripTrailing("abc  "));
        assertEquals("  abc", stripTrailing("  abc"));
        assertEquals("\t\n\rabc", stripTrailing("\t\n\rabc\t\r\n"));
    }

    @Test
    void testIsBlank() {
        assertTrue(isBlank(null));
        assertTrue(isBlank(""));
        assertTrue(isBlank("\n"));
        assertFalse(isBlank("foo"));
        assertFalse(isBlank("  foo  "));
    }

    @Test
    void testIsEmpty() {
        assertTrue(isEmpty(Optional.ofNullable(null)));
        assertFalse(isEmpty(Optional.ofNullable("foo")));
    }
}
