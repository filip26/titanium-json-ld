package com.apicatalog.rdf.io.nquad.writer;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.zip.ZipException;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;

import org.junit.Assert;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.rdf.io.nquad.NQuadsWriterTest;

public final class NQuadsWriterTestSuite {

    public static final Collection<NQuadsWriterTestCase> load() throws ZipException, IOException, URISyntaxException {
        
        try (final InputStream is = (new NQuadsWriterTest()).getClass().getResourceAsStream("manifest.json")) {

            Assert.assertNotNull(is);

            final JsonParser parser = Json.createParser(is);
            
            parser.next();
            
            return parser
                        .getArray()
                        .stream()
                        .filter(JsonUtils::isObject)
                        .map(JsonObject.class::cast)
                        .map(NQuadsWriterTestCase::of).collect(Collectors.toList());
        }
    }
}
