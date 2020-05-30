package com.apicatalog.jsonld.suite;

import java.io.IOException;
import java.io.InputStream;
import java.util.stream.Stream;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import org.junit.Assert;


public final class JsonLdManifestLoader {

    public static final String RESOURCES_BASE = "/com/github/w3c/json-ld-api/tests/"; 
        
    private final String manifestName;
    
    public JsonLdManifestLoader(String manifest) {
        this.manifestName = manifest;
    }
    
    public static JsonLdManifestLoader load(String manifest) {
        return new JsonLdManifestLoader(manifest);
    }
    
    public Stream<JsonLdTestCase> stream() throws IOException {
        
        try (InputStream is = JsonLdManifestLoader.class.getResourceAsStream(RESOURCES_BASE + manifestName)) {

            Assert.assertNotNull(is);
    
            try (final JsonParser parser = Json.createParser(is)) {
    
                if (!parser.hasNext()) {
                    return Stream.empty();
                }
                
                parser.next();
                
                final JsonObject manifest = parser.getObject();
                
                String baseUri = manifest.getString("baseIri");
                
                return manifest
                        .getJsonArray("sequence")
                            .stream()
                                .map(JsonValue::asJsonObject)
                                .map(o -> JsonLdTestCase.of(o, manifestName, baseUri));
            }
        }        
    }
}
