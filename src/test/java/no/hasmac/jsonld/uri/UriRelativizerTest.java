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

class UriRelativizerTest {

    @ParameterizedTest(name = "relativize({0}, {2}) to {1}")
    @MethodSource("data")
    void testRelativize(final String base, String expected, String url) {
        assertEquals(expected, UriRelativizer.relativize(URI.create(base), url));
    }

    static Stream<Arguments> data() {
        return Stream.of(
            arguments("http://example.com/", "person/1", "http://example.com/person/1"),
            arguments("http://example.com", "/person/1", "http://example.com/person/1"),
            arguments("https://example.com/", "relative-url", "https://example.com/relative-url"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "link", "https://w3c.github.io/json-ld-api/tests/compact/link"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "#fragment-works", "https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld#fragment-works"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "?query=works", "https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld?query=works"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../", "https://w3c.github.io/json-ld-api/tests/"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../", "https://w3c.github.io/json-ld-api/"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../parent", "https://w3c.github.io/json-ld-api/parent"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../parent#fragment", "https://w3c.github.io/json-ld-api/parent#fragment"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../../parent-parent-eq-root", "https://w3c.github.io/parent-parent-eq-root"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../../still-root", "https://w3c.github.io/still-root"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../../too-many-dots", "https://w3c.github.io/too-many-dots"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "../../../absolute", "https://w3c.github.io/absolute"),
            arguments("https://w3c.github.io/json-ld-api/tests/compact/0066-in.jsonld", "http://example.org/scheme-relative", "http://example.org/scheme-relative"),

            arguments("http://a", "/b", "http://a/b"),
            arguments("http://a/", "./", "http://a/"),
            arguments("http://a/1/2", "2", "http://a/1/2"),
            arguments("http://a/1/2/", "./", "http://a/1/2/"),
            arguments("http://a/1/2", "./", "http://a/1/2/"),
            arguments("http://a", "/", "http://a/"),
            arguments("http://a/b/c/d;p?q", "g:h", "g:h"),
            arguments("http://a/b/c/d;p?q", "g", "http://a/b/c/g"),
            arguments("http://a/b/c/?q", "g", "http://a/b/c/g"),
            arguments("http://a/b/c/d;p?q", "g/", "http://a/b/c/g/"),
            arguments("http://a/b/c/d;p?q", "../../g", "http://a/g"),
            arguments("http://a/b/c/d;p?q", "?y", "http://a/b/c/d;p?y"),
            arguments("http://a/b/c/d;p?q", "g?y", "http://a/b/c/g?y"),
            arguments("http://a/b/c/d;p?q", "#s", "http://a/b/c/d;p?q#s"),
            arguments("http://a/b/c/d;p?q", "g#s", "http://a/b/c/g#s"),
            arguments("http://a/b/c/d;p?q", "g?y#s", "http://a/b/c/g?y#s"),
            arguments("http://a/b/c/d;p?q", ";x", "http://a/b/c/;x"),
            arguments("http://a/b/c/d;p?q", "g;x", "http://a/b/c/g;x"),
            arguments("http://a/b/c/d;p?q", "g;x?y#s", "http://a/b/c/g;x?y#s"),
            arguments("http://a/b/c/d;p?q", "d;p", "http://a/b/c/d;p?q"),
            arguments("http://a/b/c/d;p?q", "../", "http://a/b/"),
            arguments("http://a/b/c/d;p?q", "../", "http://a/b/"),
            arguments("http://a/b/c/d;p?q", "../g", "http://a/b/g"),
            arguments("http://a/b/c/d;p?q", "../../", "http://a/"),
            arguments("http://a/b/c/d;p?q", "../../", "http://a/"),
            arguments("http://a/b/c/d;p?q", "../../g", "http://a/g")
        );
    }
}
