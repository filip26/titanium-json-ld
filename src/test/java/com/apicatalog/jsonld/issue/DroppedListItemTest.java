package com.apicatalog.jsonld.issue;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
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
     * @see {@link https://github.com/filip26/titanium-json-ld/issues/58}
     * @throws JsonLdError
     * @throws IOException
     */
    @Test
    public void testJsonRdfJsonCycle() throws JsonLdError, IOException {

        final Document document = readDocument("issue58-in.json");
        
        final RdfDataset dataset = JsonLd.toRdf(document).get();
        
        assertNotNull(dataset);
                
        final JsonArray result = JsonLd.fromRdf(RdfDocument.of(dataset)).nativeTypes().get();
        
        assertNotNull(result);
        
        boolean match = result.equals(document.getJsonContent().orElse(null));
        
        if (!match) {
            
            System.out.println("intermediary:");
            
            (new NQuadsWriter(new PrintWriter(System.out))).write(dataset);
            
            System.out.println("Expected:");

            Map<String, Object> properties = new HashMap<>(1);
            properties.put(JsonGenerator.PRETTY_PRINTING, true);

            JsonWriterFactory writerFactory = Json.createWriterFactory(properties);

            StringWriter writer = new StringWriter();
            
            JsonWriter jsonWriter1 = writerFactory.createWriter(writer);
            jsonWriter1.write(document.getJsonContent().orElse(null));
            jsonWriter1.close();

            writer.append("\n\n");
            writer.append("Actual:\n");

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
