package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;

import com.apicatalog.jsonld.json.JsonUtils;

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
        this(new LinkedList<>());
    }
    
    protected JsonLdContext(Collection<JsonValue> contexts) {
        this.contexts = contexts;
    }
    
    public static JsonLdContext of(final URI contextLocation) {
        return new JsonLdContext().add(contextLocation);
    }

    public static JsonLdContext of(final JsonObject contextObject) {
        return new JsonLdContext().add(contextObject);
    }
    
    public static JsonLdContext of(final Collection<JsonValue> contexts) {
        return new JsonLdContext(contexts);
    }
    
    public static JsonLdContext of(final JsonStructure jsonStructure) {
        
        if (JsonUtils.isObject(jsonStructure)) {
            return new JsonLdContext().add(jsonStructure.asJsonObject());
        }
        
        return  new JsonLdContext(new ArrayList<>(jsonStructure.asJsonArray()));        
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
