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
package no.hasmac.rdf.io.nquad;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import no.hasmac.jsonld.StringUtils;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.Keywords;
import no.hasmac.rdf.Rdf;
import no.hasmac.rdf.RdfDataset;
import no.hasmac.rdf.RdfNQuad;
import no.hasmac.rdf.RdfResource;
import no.hasmac.rdf.RdfValue;
import no.hasmac.rdf.io.error.RdfWriterException;
import no.hasmac.rdf.io.error.UnsupportedContentException;
import no.hasmac.rdf.io.nquad.writer.NQuadsWriterTestCase;
import no.hasmac.rdf.io.nquad.writer.NQuadsWriterTestSuite;
import com.google.common.base.Objects;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

class NQuadsWriterTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("data")
    void testWrite(NQuadsWriterTestCase testCase) throws IOException, URISyntaxException, RdfWriterException, UnsupportedContentException {

        assertNotNull(testCase);
        assertNotNull(testCase.getInput());
        assertNotNull(testCase.getExpected());

        String result = null;

        try (final InputStream is = getClass().getResourceAsStream(testCase.getInput())) {

            assertNotNull(is);

            JsonParser parser = Json.createParser(is);
            parser.next();

            JsonArray input = parser.getArray();
            assertNotNull(input);

            final RdfDataset dataset = Rdf.createDataset();

            for (final JsonValue statement : input) {
                dataset.add(createStatement(statement.asJsonObject()));
            }

            final ByteArrayOutputStream os = new ByteArrayOutputStream();

            Rdf.createWriter(MediaType.N_QUADS, os).write(dataset);

            result = StringUtils.stripTrailing(os.toString());
        }

        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(testCase.getExpected())))) {

            String expected = StringUtils.stripTrailing(reader.lines().collect(Collectors.joining("\n")));

            if (!Objects.equal(expected, result)) {
                System.out.println("Expected: ");
                System.out.println(expected);
                System.out.println();
                System.out.println("Result: ");
                System.out.println(result);
            }

            assertEquals(expected, result);
        }
    }

    static Stream<NQuadsWriterTestCase> data() throws IOException, URISyntaxException {
        return NQuadsWriterTestSuite.load();
    }

    private static RdfNQuad createStatement(final JsonObject value) {

        RdfResource subject = Rdf.createResource(value.getString("subject"));

        RdfResource predicate = Rdf.createResource(value.getString("predicate"));

        RdfValue object = createObject(value.get("object"));

        RdfResource graph = null;

        if (value.containsKey("graph")) {
            graph = Rdf.createResource(value.getString("graph"));
        }

        return Rdf.createNQuad(subject, predicate, object, graph);
    }

    private static RdfValue createObject(final JsonValue value) {

        if (JsonUtils.isString(value)) {

            final String stringValue = ((JsonString)value).getString();

            return Rdf.createValue(stringValue);
        }

        JsonObject object = value.asJsonObject();

        if (object.containsKey(Keywords.TYPE)) {
            return Rdf.createTypedString(object.getString(Keywords.VALUE), object.getString(Keywords.TYPE));
        }

        if (object.containsKey(Keywords.LANGUAGE)) {
            return Rdf.createLangString(object.getString(Keywords.VALUE), object.getString(Keywords.LANGUAGE));
        }

        throw new IllegalStateException();
    }
}
