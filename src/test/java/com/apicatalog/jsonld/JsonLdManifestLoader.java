package com.apicatalog.jsonld;

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
        
    private final String path;
    
    public JsonLdManifestLoader(String path) {
        this.path = path;
    }
    
    public static JsonLdManifestLoader load(String path) {
        return new JsonLdManifestLoader(RESOURCES_BASE + path);
    }
    
    public Stream<JsonLdTestCase> stream() throws IOException {
        
        try (InputStream is = JsonLdManifestLoader.class.getResourceAsStream(path)) {

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
                                .map(o -> JsonLdTestCase.of(o, baseUri));
            }
        }        
    }
}
