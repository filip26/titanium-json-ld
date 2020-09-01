package com.apicatalog.jsonld.issue;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.stream.JsonGenerator;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.nquad.NQuadsWriter;

public class DroppedListItemTest {

    /**
     * @see <a href="https://github.com/filip26/titanium-json-ld/issues/58">Issue #58</a>
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    public void testJsonRdfJsonCycle() throws JsonLdError, IOException {

        final Document document = readDocument("issue58-2-in.json");
        
        final RdfDataset dataset = JsonLd.toRdf(document).get();
        
        (new NQuadsWriter(new PrintWriter(System.out))).write(dataset);
        
        assertNotNull(dataset);
                
        final JsonArray result = JsonLd.fromRdf(RdfDocument.of(dataset)).nativeTypes().get();
        
        assertNotNull(result);
        
        final Document expected = readDocument("issue58-2-out.json");
        
        boolean match = result.equals(expected.getJsonContent().orElse(null));
        
        if (!match) {
            
            System.out.println("Intermediary:");
            
            (new NQuadsWriter(new PrintWriter(System.out))).write(dataset);
            
            System.out.println("\nExpected:");

            JsonWriterFactory writerFactory = Json.createWriterFactory(Map.of(JsonGenerator.PRETTY_PRINTING, true));

            StringWriter writer = new StringWriter();
            
            JsonWriter jsonWriter1 = writerFactory.createWriter(writer);
            jsonWriter1.write(expected.getJsonContent().orElse(null));
            jsonWriter1.close();

            writer.append("\n\nActual:\n");

            JsonWriter jsonWriter2 = writerFactory.createWriter(writer);
            jsonWriter2.write(result);
            jsonWriter2.close();

            System.out.print(writer.toString());
            System.out.println();
            System.out.println();
        }
        
        assertTrue(match);
        
    }

    @Test
    public void testFromRdfOneItemList() throws JsonLdError, IOException {

        final JsonArray result;
        
        try (final InputStream is = getClass().getResourceAsStream("issue58-in.nq")) {
            
            assertNotNull(is);
            
            result = JsonLd.fromRdf(RdfDocument.of(is)).nativeTypes().get();

            assertNotNull(result);
        }
                
        final Document expected = readDocument("issue58-out.json");
        
        boolean match = result.equals(expected.getJsonContent().orElse(null));
        
        if (!match) {
            
            System.out.println("\nExpected:");

            JsonWriterFactory writerFactory = Json.createWriterFactory(Map.of(JsonGenerator.PRETTY_PRINTING, true));

            StringWriter writer = new StringWriter();
            
            JsonWriter jsonWriter1 = writerFactory.createWriter(writer);
            jsonWriter1.write(expected.getJsonContent().orElse(null));
            jsonWriter1.close();

            writer.append("\n\nActual:\n");

            JsonWriter jsonWriter2 = writerFactory.createWriter(writer);
            jsonWriter2.write(result);
            jsonWriter2.close();

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
