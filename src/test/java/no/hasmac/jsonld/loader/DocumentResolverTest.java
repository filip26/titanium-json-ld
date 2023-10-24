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
package no.hasmac.jsonld.loader;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.InputStream;

import org.junit.jupiter.api.Test;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.http.media.MediaType;

class DocumentResolverTest {

    @Test
    void test1() throws JsonLdError {
        DocumentReader<InputStream> reader = (new DocumentResolver()).getReader(MediaType.N_QUADS);
        assertNotNull(reader);
    }

    @Test
    void test2() throws JsonLdError {
        DocumentReader<InputStream> reader = (new DocumentResolver()).getReader(MediaType.JSON_LD);
        assertNotNull(reader);
    }

    @Test
    void test3() throws JsonLdError {
        DocumentReader<InputStream> reader = (new DocumentResolver()).getReader(MediaType.JSON);
        assertNotNull(reader);
    }

    @Test
    void test4() throws JsonLdError {
        DocumentReader<InputStream> reader = (new DocumentResolver()).getReader(MediaType.of("application", "test+json"));
        assertNotNull(reader);
    }

    @Test
    void test5() throws JsonLdError {
        assertThrows(JsonLdError.class, () -> (new DocumentResolver()).getReader(MediaType.HTML));
    }

    @Test
    void test6() throws JsonLdError {
        DocumentResolver resolver = new DocumentResolver();
        resolver.setFallbackContentType(MediaType.JSON);
        DocumentReader<InputStream> reader = resolver.getReader(MediaType.of("text/plain"));
        assertNotNull(reader);
    }

    @Test
    void test7() throws JsonLdError {
        DocumentResolver resolver = new DocumentResolver();
        resolver.setFallbackContentType(MediaType.XHTML);
        assertThrows(JsonLdError.class, () -> resolver.getReader(MediaType.of("text/plain")));
    }

    @Test
    void test8() throws JsonLdError {
        DocumentResolver resolver = new DocumentResolver();
        resolver.setFallbackContentType(MediaType.ANY);
        DocumentReader<InputStream> reader = resolver.getReader(MediaType.of("text/plain"));
        assertNotNull(reader);
    }

    @Test
    void test9() throws JsonLdError {
        DocumentResolver resolver = new DocumentResolver();
        resolver.setFallbackContentType(MediaType.XHTML);
        DocumentReader<InputStream> reader = resolver.getReader(MediaType.of("text/plain+json; charset=utf-8"));
        assertNotNull(reader);
    }

}
