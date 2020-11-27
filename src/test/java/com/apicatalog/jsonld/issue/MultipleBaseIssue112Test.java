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

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.error.RdfWriterException;
import com.apicatalog.rdf.io.error.UnsupportedContentException;

public class MultipleBaseIssue112Test {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/112">Issue #112</a>
     * @throws JsonLdError
     * @throws IOException
     * @throws UnsupportedContentException 
     * @throws RdfWriterException 
     */
    @Test
    public void testJsonRdfJsonCycle() throws JsonLdError, IOException, RdfWriterException, UnsupportedContentException {

        final Document document = readJsonDocument("issue112-in.json");
        
        final RdfDataset dataset = JsonLd.toRdf(document).get();
        
        assertNotNull(dataset);
                
        final RdfDataset expected = readRdfDocument("issue112-out.nq").getRdfContent().orElse(null);
        
        assertNotNull(expected);
        
        boolean match = RdfComparison.equals(dataset, expected);
        
        if (!match) {
                        
            System.out.println("\nExpected:");

            StringWriter writer = new StringWriter();

            Rdf.createWriter(MediaType.N_QUADS, writer).write(expected);

            writer.append("\n\nActual:\n");

            Rdf.createWriter(MediaType.N_QUADS, writer).write(dataset);

            System.out.print(writer.toString());
            System.out.println();
            System.out.println();
        }
        
        assertTrue(match);
        
    }

    private final JsonDocument readJsonDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }
    
    private final RdfDocument readRdfDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return RdfDocument.of(is);
        }
    }    
}
