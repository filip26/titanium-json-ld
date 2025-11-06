package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.Comparison;
import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdTestSuite;
import com.apicatalog.jsonld.Options;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.model.RdfQuadSet;
import com.apicatalog.rdf.nquads.NQuadsReader;
import com.apicatalog.rdf.nquads.NQuadsReaderException;
import com.apicatalog.rdf.primitive.flow.QuadAcceptor;
import com.apicatalog.rdf.primitive.set.OrderedQuadSet;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

class NativeInputTest {

    @Test
    void testExpand() throws JsonLdException, IOException {

        final var result = JsonLd.expand(PL_2_COMPACTED, Options.newOptions());
        assertNotNull(result);

        var match = Comparison.equals(
                result,
                PL_2_EXPANDED,
                NativeAdapter.instance());

        assertTrue(match);
    }

    @Test
    void testCompact() throws JsonLdException, IOException {

        final var result = JsonLd.compact(PL_2_EXPANDED, PL_2_CONTEXT, Options.newOptions());
        assertNotNull(result);

        var match = Comparison.equals(
                result,
                PL_2_COMPACTED,
                NativeAdapter.instance());

        assertTrue(match);
    }

    @Test
    void testFlatten() throws JsonLdException, IOException {

        final var result = JsonLd.flatten(PL_2_COMPACTED, Options.newOptions());
        assertNotNull(result);

        final var expected = readJson("/com/apicatalog/jsonld/test/pl-2-flattened.json");
        assertNotNull(expected);

        var match = Comparison.equals(
                result,
                NativeAdapter.instance(),
                expected.node(),
                expected.adapter());

        assertTrue(match);
    }

    @Test
    void testFrame() throws JsonLdException, IOException {

        final var result = JsonLd.frame(PL_2_COMPACTED, PL_2_FRAME, Options.newOptions());
        assertNotNull(result);

//        final var expected = readJson("/com/apicatalog/jsonld/test/pl-2-flattened.json");
//        assertNotNull(expected);
        var match = Comparison.equals(
                result,
                PL_2_COMPACTED,
                NativeAdapter.instance());

        assertTrue(match);
    }

    @Test
    void testToRdf() throws JsonLdException, IOException, NQuadsReaderException, RdfConsumerException {

        final var set = new OrderedQuadSet();

        JsonLd.toRdf(PL_2_COMPACTED, new QuadAcceptor(set), Options.newOptions());

        final var expected = readNQuads("/com/apicatalog/jsonld/test/pl-2-quads.nq");
        assertNotNull(expected);

        var match = RdfComparison.equals(set, expected);
        assertTrue(match);
    }

    static Map<String, ?> PL_2_COMPACTED = Map.of(
            "@context", Map.of(
                    "ical", "http://www.w3.org/2002/12/cal/ical#",
                    "xsd", "http://www.w3.org/2001/XMLSchema#",
                    "ical:dtstart", Map.of("@type", "xsd:dateTime")),
            "ical:summary", "Lady Gaga Concert",
            "ical:location", "New Orleans Arena, New Orleans, Louisiana, USA",
            "ical:dtstart", "2011-04-09T20:00:00Z");

    static Collection<?> PL_2_EXPANDED = List.of(Map.of(
            "http://www.w3.org/2002/12/cal/ical#dtstart", List.of(
                    Map.of(
                            "@type", "http://www.w3.org/2001/XMLSchema#dateTime",
                            "@value", "2011-04-09T20:00:00Z")),
            "http://www.w3.org/2002/12/cal/ical#location", List.of(
                    Map.of(
                            "@value", "New Orleans Arena, New Orleans, Louisiana, USA")),
            "http://www.w3.org/2002/12/cal/ical#summary", List.of(
                    Map.of(
                            "@value", "Lady Gaga Concert"))));

    static Map<String, ?> PL_2_CONTEXT = Map.of(
            "@context", Map.of(
                    "ical", "http://www.w3.org/2002/12/cal/ical#",
                    "xsd", "http://www.w3.org/2001/XMLSchema#",
                    "ical:dtstart", Map.of(
                            "@type", "xsd:dateTime")));

    static Map<String, ?> PL_2_FRAME = Map.of(
            "@context", Map.of(
                    "ical", "http://www.w3.org/2002/12/cal/ical#",
                    "xsd", "http://www.w3.org/2001/XMLSchema#",
                    "ical:dtstart", Map.of(
                            "@type", "xsd:dateTime")),
            "ical:location", "New Orleans Arena, New Orleans, Louisiana, USA");

    private static final TreeIO readJson(final String name) throws JsonLdException, IOException {
        try (final var is = NativeInputTest.class.getResourceAsStream(name)) {
            return JsonLdTestSuite.PARSER.parse(is);
        }
    }

    public static RdfQuadSet readNQuads(String name) throws NQuadsReaderException, RdfConsumerException, JsonLdException, IOException {
        try (final var is = NativeInputTest.class.getResourceAsStream(name)) {
            final var content = new OrderedQuadSet();
            new NQuadsReader(new InputStreamReader(is)).provide(new QuadAcceptor(content));
            return content;
        }
    }
}
