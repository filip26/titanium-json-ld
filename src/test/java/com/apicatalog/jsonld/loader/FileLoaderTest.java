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
package com.apicatalog.jsonld.loader;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.Assert;
import org.junit.Test;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.http.media.MediaType;

public class FileLoaderTest {

    @Test
    public void testLoadNQuads() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.nq");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.N_QUADS.match(document.getContentType()));
    }

    @Test
    public void testLoadJson() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.json");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON.match(document.getContentType()));
    }

    @Test
    public void testLoadJsonLd() throws URISyntaxException, JsonLdError {
        
        URL fileUrl = getClass().getResource("document.jsonld");
        
        Assert.assertNotNull(fileUrl);
        
        Document document = (new FileLoader()).loadDocument(fileUrl.toURI(), new DocumentLoaderOptions());
        
        Assert.assertNotNull(document);
        Assert.assertTrue(MediaType.JSON_LD.match(document.getContentType()));
    }

    @Test
    public void testLoadHtml() throws URISyntaxException {
        
        URL fileUrl = getClass().getResource("document.html");
        
        Assert.assertNotNull(fileUrl);
        
        Assert.assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(fileUrl.toURI(), new DocumentLoaderOptions()));
    }

    @Test
    public void testUnsupportedScheme() throws URISyntaxException {        
        Assert.assertThrows(JsonLdError.class, () -> new FileLoader().loadDocument(URI.create("https://github.com/"), new DocumentLoaderOptions()));
    }

}
