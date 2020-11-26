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
package com.apicatalog.rdf.io.nquad;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.io.error.RdfWriterException;
import com.apicatalog.rdf.io.error.UnsupportedContentException;
import com.apicatalog.rdf.io.nquad.writer.NQuadsWriterTestCase;
import com.apicatalog.rdf.io.nquad.writer.NQuadsWriterTestSuite;
import com.google.common.base.Objects;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

@RunWith(Parameterized.class)
public class NQuadsWriterTest {

    @Parameterized.Parameter(0)
    public NQuadsWriterTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;

    @Test
    public void testWrite() throws IOException, URISyntaxException, RdfWriterException, UnsupportedContentException {

        Assert.assertNotNull(testCase);
        Assert.assertNotNull(testCase.getInput());
        Assert.assertNotNull(testCase.getExpected());
        
        String result = null;
        
        try (final InputStream is = getClass().getResourceAsStream(testCase.getInput())) {

            Assert.assertNotNull(is);

            JsonParser parser = Json.createParser(is);
            parser.next();
            
            JsonArray input = parser.getArray();
            Assert.assertNotNull(input);
            
            final RdfDataset dataset = Rdf.createDataset();
            
            for (final JsonValue statement : input) {
                dataset.add(createStatement(statement.asJsonObject()));    
            }
            
            final ByteArrayOutputStream os = new ByteArrayOutputStream();
            
            Rdf.createWriter(MediaType.N_QUADS, os).write(dataset);
            
            result = os.toString().stripTrailing();
        }
        
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(testCase.getExpected())))) {
            
            String expected = reader.lines().collect(Collectors.joining("\n")).stripTrailing();
            
            if (!Objects.equal(expected, result)) {
                System.out.println("Expected: ");
                System.out.println(expected);
                System.out.println();
                System.out.println("Result: ");
                System.out.println(result);
            }
            
            Assert.assertEquals(expected, result);
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws IOException, URISyntaxException {
        return NQuadsWriterTestSuite
                .load()
                .stream()            
                .map(o -> new Object[] {o, o.getId(), o.getName()})
                .collect(Collectors.toList());
    }
    
    private static final RdfNQuad createStatement(final JsonObject value) {
        
        RdfResource subject = Rdf.createResource(value.getString("subject"));

        RdfResource predicate = Rdf.createResource(value.getString("predicate"));

        RdfValue object = createObject(value.get("object"));
            
        RdfResource graph = null;
        
        if (value.containsKey("graph")) {
            graph = Rdf.createResource(value.getString("graph"));
        }
        
        return Rdf.createNQuad(subject, predicate, object, graph);
    }   
    
    private static final RdfValue createObject(final JsonValue value) {
        
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
