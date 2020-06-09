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

    public static final String JSON_LD_API_BASE = "/com/github/w3c/json-ld-api/tests/";
    public static final String JSON_LD_FRAMING_BASE = "/com/github/w3c/json-ld-framing/tests/";
        
    private final String manifestName;
    private final String manifestBase;
    
    private JsonLdManifestLoader(final String manifestBase, final String manifestName) {
        this.manifestBase = manifestBase;
        this.manifestName = manifestName;
    }
    
    public static JsonLdManifestLoader load(final String manifestBase, final String manifestName) {
        return new JsonLdManifestLoader(manifestBase, manifestName);
    }
    
    public Stream<JsonLdTestCase> stream() throws IOException {
        
        try (InputStream is = JsonLdManifestLoader.class.getResourceAsStream(manifestBase + manifestName)) {

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
