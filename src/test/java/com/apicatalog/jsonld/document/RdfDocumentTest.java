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
package com.apicatalog.jsonld.document;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.JsonValue;

public class RdfDocumentTest {
    
    private static final String NQ_STATEMENT = "<http://example/s> <http://example/p> <http://example/o> <http://example/g> .";
    
    @Test
    public void test1() {
        Document document = RdfDocument.of(Rdf.createDataset());
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isPresent());
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(0, document.getRdfContent().get().size());
    }

    @Test
    public void test2() {
        Document document = RdfDocument.of(MediaType.N_QUADS, Rdf.createDataset());
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isPresent());
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(0, document.getRdfContent().get().size());
    }

    @Test
    public void test3() throws JsonLdError {
        Document document = RdfDocument.of(new ByteArrayInputStream(NQ_STATEMENT.getBytes()));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isPresent());
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(1, document.getRdfContent().get().size());
    }

    @Test
    public void test4() throws JsonLdError {
        Document document = RdfDocument.of(new InputStreamReader(new ByteArrayInputStream(NQ_STATEMENT.getBytes())));
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
        Assert.assertTrue(document.getRdfContent().isPresent());
        Assert.assertTrue(document.getJsonContent().isEmpty());
        Assert.assertTrue(document.getProfile().isEmpty());
        Assert.assertEquals(1, document.getRdfContent().get().size());
    }
    
    @Test
    public void testi1() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((InputStream)null));
    }

    @Test
    public void testi2() {
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((RdfDataset)null));
    }

    @Test
    public void testi3() throws JsonLdError {
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of((Reader)null));
    }
    
    @Test
    public void testi4() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes());
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, is));
    }

    @Test
    public void testi5() {
        final RdfDataset dataset = Rdf.createDataset();
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, dataset));
    }

    @Test
    public void testi6() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream(JsonValue.EMPTY_JSON_ARRAY.toString().getBytes()));
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(null, reader));
    }
    
    @Test
    public void testi7() throws JsonLdError {
        final InputStream is = new ByteArrayInputStream("{ bad json".getBytes());
        Assert.assertThrows(JsonLdError.class, () -> RdfDocument.of(is));
    }

    @Test
    public void testi8() throws JsonLdError {
        final Reader reader = new InputStreamReader(new ByteArrayInputStream("n".getBytes()));
        Assert.assertThrows(JsonLdError.class, () -> RdfDocument.of(reader));
    }
    
    @Test
    public void test9() {
        final MediaType mediaType = MediaType.of("application/custom+json;profile=https://example.org/profile");
        final RdfDataset dataset = Rdf.createDataset();
        Assert.assertThrows(IllegalArgumentException.class, () -> RdfDocument.of(mediaType, dataset));
    }
}
