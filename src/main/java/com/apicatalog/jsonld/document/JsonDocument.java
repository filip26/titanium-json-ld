package com.apicatalog.jsonld.document;

import java.io.Reader;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.utils.JsonUtils;

public final class JsonDocument implements Document {

    private JsonStructure structure;

    public JsonDocument(final JsonStructure structue) {
        this.structure = structue;
    }

    public static final Document parse(final Reader reader)  throws JsonLdError {
        
        try (final JsonParser parser = Json.createParser(reader)) {

            if (!parser.hasNext()) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
            }

            parser.next();

            JsonValue root = parser.getValue();

            if (JsonUtils.isArray(root)) {
                return new JsonDocument(root.asJsonArray());
            }

            if (JsonUtils.isObject(root)) {
                return new JsonDocument(root.asJsonObject());
            }

            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    
    @Override
    public JsonStructure asJsonStructure() throws JsonLdError {
        return structure;
    }

}
