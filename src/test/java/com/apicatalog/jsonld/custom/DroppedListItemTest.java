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
package com.apicatalog.jsonld.custom;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.test.JsonLdTestRunnerJunit;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.JsonArray;

public class DroppedListItemTest {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/58">Issue #58</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    public void testJsonRdfJsonCycle() throws JsonLdError, IOException {

        final Document document = readDocument("/com/apicatalog/jsonld/test/issue58-2-in.json");
        
        final RdfDataset dataset = JsonLd.toRdf(document).get();
        
        assertNotNull(dataset);
                
        final JsonArray result = JsonLd.fromRdf(RdfDocument.of(dataset)).nativeTypes().get();
        
        assertNotNull(result);
        
        final Document expected = readDocument("/com/apicatalog/jsonld/test/issue58-2-out.json");
        
        assertTrue(JsonLdTestRunnerJunit.compareJson("JSON to RDF to JSON", result, expected.getJsonContent().orElse(null)));
        
    }

    @Test
    public void testFromRdfOneItemList() throws JsonLdError, IOException {

        final JsonArray result;
        
        try (final InputStream is = getClass().getResourceAsStream("/com/apicatalog/jsonld/test/issue58-in.nq")) {
            
            assertNotNull(is);
            
            result = JsonLd.fromRdf(RdfDocument.of(is)).ordered().get();

            assertNotNull(result);
        }
                
        final Document expected = readDocument("/com/apicatalog/jsonld/test/issue58-out.json");

        assertNotNull(expected);
        
        assertTrue(JsonLdTestRunnerJunit.compareJson("fromRdf: one item list", result, expected.getJsonContent().orElse(null)));
        
    }

    private final Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }    
}
