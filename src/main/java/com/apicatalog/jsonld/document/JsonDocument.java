package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.io.Reader;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.json.JsonUtils;

public final class JsonDocument implements Document {

    private JsonStructure structure;

    protected JsonDocument(final JsonStructure structue) {
        this.structure = structue;
    }

    public static final Document parse(final InputStream is)  throws JsonLdError {
        try (final JsonParser parser = Json.createParser(is)) {

            return doParse(parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    public static final Document parse(final Reader reader)  throws JsonLdError {
        
        try (final JsonParser parser = Json.createParser(reader)) {

            return doParse(parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    private static final Document doParse(final JsonParser parser) throws JsonLdError {
        
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
    }
    
    @Override
    public Optional<JsonStructure> getJsonStructure() {
        return Optional.of(structure);
    }

    @Override
    public Optional<byte[]> getRawPayload() {
        return Optional.empty();
    }

    @Override
    public boolean isJsonStructure() {
        return true;
    }
    
    @Override
    public boolean isRawPayload() {
        return false;
    }

}
