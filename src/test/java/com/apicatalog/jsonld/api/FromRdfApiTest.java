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
package com.apicatalog.jsonld.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URI;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.Rdf;

import jakarta.json.JsonArray;

class FromRdfApiTest {

    public static final MockLoader MOCK_LOADER = new MockLoader(Rdf.createDataset());
    
    @Test    
    void test1() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf(RdfDocument.of(Rdf.createDataset())).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }
        
    @Test    
    void test3() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf("https://example.com").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test    
    void test4() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf(URI.create("https://example.com")).loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test    
    void test5() throws JsonLdError {
        JsonArray result = JsonLd.fromRdf("\thttps://example.com  ").loader(MOCK_LOADER).get();
        assertNotNull(result);
        assertEquals(0, result.size());
    }
}
