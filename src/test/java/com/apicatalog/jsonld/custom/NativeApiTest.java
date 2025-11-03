package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdComparison;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdTestSuite;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.tree.io.java.NativeAdapter;

class NativeApiTest {

    @Test
    void testExpand() throws JsonLdException, IOException {

        final var result = JsonLd.expand(DATA_1_IN, JsonLdOptions.newOptions());
        assertNotNull(result);

        final var expected = readJson("/com/apicatalog/jsonld/test/pl-2-expanded.json");
        assertNotNull(expected);

        var match = JsonLdComparison.equals(result, NativeAdapter.instance(), expected.node(), expected.adapter());

        assertTrue(match);
    }

    @Test
    void testFlatten() throws JsonLdException, IOException {

        final var result = JsonLd.flatten(DATA_1_IN, JsonLdOptions.newOptions());
        assertNotNull(result);

        final var expected = readJson("/com/apicatalog/jsonld/test/pl-2-flattened.json");
        assertNotNull(expected);

        var match = JsonLdComparison.equals(result, NativeAdapter.instance(), expected.node(), expected.adapter());

        assertTrue(match);
    }

    @Test
    void testToRdf() throws JsonLdException, IOException, NQuadsReaderException, RdfConsumerException {

        final var set = new OrderedQuadSet();

        JsonLd.toRdf(DATA_1_IN, new QuadAcceptor(set), JsonLdOptions.newOptions());

        final var expected = readNQuads("/com/apicatalog/jsonld/test/pl-2-quads.nq");
        assertNotNull(expected);

        var match = RdfComparison.equals(set, expected);
        assertTrue(match);
    }

    static Map<String, ?> DATA_1_IN = Map.of(
            "@context", Map.of(
                    "ical", "http://www.w3.org/2002/12/cal/ical#",
                    "xsd", "http://www.w3.org/2001/XMLSchema#",
                    "ical:dtstart", Map.of("@type", "xsd:dateTime")),
            "ical:summary", "Lady Gaga Concert",
            "ical:location", "New Orleans Arena, New Orleans, Louisiana, USA",
            "ical:dtstart", "2011-04-09T20:00:00Z");

    private static final PolyNode readJson(final String name) throws JsonLdException, IOException {
        try (final var is = NativeApiTest.class.getResourceAsStream(name)) {
            return JsonLdTestSuite.JAKARTA_PARSER.parse(is);
        }
    }

    public static RdfQuadSet readNQuads(String name) throws NQuadsReaderException, RdfConsumerException, JsonLdException, IOException {
        try (final var is = NativeApiTest.class.getResourceAsStream(name)) {
            var content = new OrderedQuadSet();
            new NQuadsReader(new InputStreamReader(is)).provide(new QuadAcceptor(content));
            return content;
        }
    }
}
