package com.apicatalog.jsonld.issue;

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
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;

public class ToRdfIssue61Test {

    
    @Test
    public void testToRdfMissingTriples() throws JsonLdError, IOException {

        final Document document = readDocument("issue61-in.json");
        final Document context = readDocument("issue61-context.json");
        
        final RdfDataset result = JsonLd.toRdf(document).context(context).get();
        
        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("issue61-out.nq")) {
            
            assertNotNull(is);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));
            
            assertTrue(match);            
        }
    }

    private final Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }
    
}
