package com.apicatalog.jsonld.issue;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.io.InputStream;

import javax.json.JsonArray;
import javax.json.JsonValue;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;

public class NativeTypesIssue64Test {

    
    @Test
    public void testFromRdfNativeTypes() throws JsonLdError, IOException {

        final JsonArray result;
        
        try (final InputStream is = getClass().getResourceAsStream("issue64-in.nq")) {
            assertNotNull(is);
            
            result = JsonLd.fromRdf(RdfDocument.of(is)).nativeTypes().get();

            assertNotNull(result);
        }
        
        final JsonValue expected;
        
        try (final InputStream is = getClass().getResourceAsStream("issue64-out.json")) {
            assertNotNull(is);

            expected = JsonDocument.of(is).getJsonContent().orElse(null);
            
            assertNotNull(expected);
        }

        assertEquals(expected, result);
    }
    
}
