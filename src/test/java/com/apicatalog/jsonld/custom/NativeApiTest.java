package com.apicatalog.jsonld.custom;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;

class NativeApiTest {

    @Test
    void testExpand() throws JsonLdException, IOException {

        var x = JsonLd.expand(DATA_1_IN, JsonLdOptions.newOptions());

        System.out.println(x);
//        var b = new ByteArrayOutputStream();

        // TODO
//        var y = new JakartaWriter(Json.createGeneratorFactory(Map.of()).createGenerator(b)).node(x, NativeAdapter.instance());

//        System.out.println(y);
    }

    @Test
    void testToRdf() throws JsonLdException, IOException {
        
        final var set = new OrderedQuadSet();
        
        JsonLd.toRdf(DATA_1_IN, new QuadAcceptor(set), JsonLdOptions.newOptions());
        
        System.out.println(set);
    }

    static Map<String, ?> DATA_1_IN = Map.of(
            "@context", Map.of(
                    "ical", "http://www.w3.org/2002/12/cal/ical#",
                    "xsd", "http://www.w3.org/2001/XMLSchema#",
                    "ical:dtstart", Map.of("@type", "xsd:dateTime")),
            "ical:summary", "Lady Gaga Concert",
            "ical:location", "New Orleans Arena, New Orleans, Louisiana, USA",
            "ical:dtstart", "2011-04-09T20:00:00Z");

}
