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
package com.apicatalog.jsonld.issue;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Map;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public class BlankNodes118Test {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/118">Issue #118</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    public void testBlankNodeNotation() throws JsonLdError, IOException {

        final RdfDataset dataset = Rdf.createDataset()
                                        .add(Rdf.createTriple(
                                                    Rdf.createIRI("urn:s1"), 
                                                    Rdf.createIRI("urn:p1"),
                                                    Rdf.createBlankNode("bn1")
                                                ));

        final JsonArray result = JsonLd.fromRdf(RdfDocument.of(dataset)).get();
        
        assertNotNull(result);
        
        final Document expected = readDocument("issue118-out.json");
        
        assertNotNull(expected);
        
        boolean match = result.equals(expected.getJsonContent().orElse(null));
        
        if (!match) {
            
            System.out.println("\nExpected:");

            JsonWriterFactory writerFactory = Json.createWriterFactory(Map.of(JsonGenerator.PRETTY_PRINTING, true));

            StringWriter writer = new StringWriter();
            
            try (JsonWriter jsonWriter = writerFactory.createWriter(writer)) {
                jsonWriter.write(expected.getJsonContent().orElse(null));
            }

            writer.append("\n\nActual:\n");

            try (final JsonWriter jsonWriter = writerFactory.createWriter(writer)) {
                jsonWriter.write(result);                
            };
            
            System.out.print(writer.toString());
            System.out.println();
            System.out.println();
        }
        
        assertTrue(match);
    }
    
    private final Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }
}
