package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Collection;
import java.util.LinkedList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonValue;

/**
 * The {@link JsonLdContext} interface is used to refer to a value that may be a
 * {@link JsonLdRecord}, a sequence of {@link JsonLdRecord}, or a string
 * representing an <code>IRI</code>, which can be dereferenced to retrieve a
 * valid JSON document.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#webidl-1159289741">JsonLdContext
 *      Specification</a>
 *
 */
public final class JsonLdContext {

    private final Collection<JsonValue> contexts;
    
    protected JsonLdContext() {
        this.contexts = new LinkedList<>();
    }
    
    public static JsonLdContext of(final URI contextLocation) {
        return new JsonLdContext().add(contextLocation);
    }

    public static JsonLdContext of(final JsonObject contextObject) {
        return new JsonLdContext().add(contextObject);
    }

    public JsonLdContext add(URI contextLocation) {
        contexts.add(Json.createValue(contextLocation.toString()));
        return this;
    }

    public JsonLdContext add(JsonObject contextObject) {
        contexts.add(contextObject);
        return this;
    }

    public JsonArray asJsonArray() {
        
        final JsonArrayBuilder builder = Json.createArrayBuilder();
        
        contexts.forEach(builder::add);
        
        return builder.build();
    }
}
