package com.apicatalog.jsonld.document;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;

public interface Document {

    boolean isJsonStructure();
    boolean isRawPayload();
    
    JsonStructure getJsonStructure() throws JsonLdError;

    byte[] getRawPayload() throws JsonLdError;
    
    public static Document of(final byte[] payload) {
        return new RawPayload(payload);
    }
    
    public static Document of(final JsonStructure structure) {
        return new JsonDocument(structure);
    }
}
