package com.apicatalog.rdf.io.nquad;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.writer.NQuadsWriterTestCase;
import com.apicatalog.rdf.io.nquad.writer.NQuadsWriterTestSuite;
import com.google.common.base.Objects;

@RunWith(Parameterized.class)
public class NQuadsWriterTest {

    @Parameterized.Parameter(0)
    public NQuadsWriterTestCase testCase;

    @Parameterized.Parameter(1)
    public String testId;
    
    @Parameterized.Parameter(2)
    public String testName;

    @Test
    public void testWrite() throws IOException, URISyntaxException, NQuadsWriterException, UnsupportedFormatException {

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
            
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            
            Rdf.createWriter(os, RdfFormat.N_QUADS).write(dataset);
            
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
