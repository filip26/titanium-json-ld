package com.apicatalog.jsonld.api;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;

class NativeObjectsTest {

    @Test
    void testExpand() throws JsonLdException, IOException {

        var x = JsonLd.expand(Map.of(
                "@context", Map.of(
                        "ical", "http://www.w3.org/2002/12/cal/ical#",
                        "xsd", "http://www.w3.org/2001/XMLSchema#",
                        "ical:dtstart", Map.of("@type", "xsd:dateTime")),
                "ical:summary", "Lady Gaga Concert",
                "ical:location", "New Orleans Arena, New Orleans, Louisiana, USA",
                "ical:dtstart", "2011-04-09T20:00:00Z"), JsonLdOptions.newOptions());

        System.out.println(x);
        var b = new ByteArrayOutputStream();

        // TODO
//        var y = new JakartaWriter(Json.createGeneratorFactory(Map.of()).createGenerator(b)).node(x, NativeAdapter.instance());

//        System.out.println(y);

    }

}
