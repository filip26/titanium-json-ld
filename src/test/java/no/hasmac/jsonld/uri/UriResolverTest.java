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
package no.hasmac.jsonld.uri;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.net.URI;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class UriResolverTest {

    @ParameterizedTest(name = "resolve({0}, {1}) to {2}")
    @MethodSource("data")
    void testResolve(final String base, String relative, String expected) {
        assertEquals(expected, UriResolver.resolve(URI.create(base), relative));
    }

    static Stream<Arguments> data() {
        return Stream.of(
            arguments("file:///a/bb/ccc/d;p?q,", "g", "file:///a/bb/ccc/g"),
            arguments("file:///a/bb/ccc/d;p?q,", "/g", "file:///g"),
            arguments("http://a/b/c/d;p?q", "g:h", "g:h"),
            arguments("http://a/b/c/d;p?q", "g", "http://a/b/c/g"),
            arguments("http://a/b/c/d;p?q", "./g", "http://a/b/c/g"),
            arguments("http://a/b/c/d;p?q", "g/", "http://a/b/c/g/"),
            arguments("http://a/b/c/d;p?q", "/g", "http://a/g"),
            arguments("http://a/b/c/d;p?q", "//g", "http://g"),
            arguments("http://a/b/c/d;p?q", "?y", "http://a/b/c/d;p?y"),
            arguments("http://a/b/c/d;p?q", "g?y", "http://a/b/c/g?y"),
            arguments("http://a/b/c/d;p?q", "#s", "http://a/b/c/d;p?q#s"),
            arguments("http://a/b/c/d;p?q", "g#s", "http://a/b/c/g#s"),
            arguments("http://a/b/c/d;p?q", "g?y#s", "http://a/b/c/g?y#s"),
            arguments("http://a/b/c/d;p?q", ";x", "http://a/b/c/;x"),
            arguments("http://a/b/c/d;p?q", "g;x", "http://a/b/c/g;x"),
            arguments("http://a/b/c/d;p?q", "g;x?y#s", "http://a/b/c/g;x?y#s"),
            arguments("http://a/b/c/d;p?q", "", "http://a/b/c/d;p?q"),
            arguments("http://a/b/c/d;p?q", ".", "http://a/b/c/"),
            arguments("http://a/b/c/d;p?q", "./", "http://a/b/c/"),
            arguments("http://a/b/c/d;p?q", "..", "http://a/b/"),
            arguments("http://a/b/c/d;p?q", "../", "http://a/b/"),
            arguments("http://a/b/c/d;p?q", "../g", "http://a/b/g"),
            arguments("http://a/b/c/d;p?q", "../..", "http://a/"),
            arguments("http://a/b/c/d;p?q", "../../", "http://a/"),
            arguments("http://a/b/c/d;p?q", "../../g", "http://a/g"),
            arguments("http://a/", "", "http://a/"),
            arguments("http://a/b/c", "/b", "http://a/b")
        );
    }
}
