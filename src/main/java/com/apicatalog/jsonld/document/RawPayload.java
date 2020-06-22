package com.apicatalog.jsonld.document;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;

public final class RawPayload implements Document {

    private final byte[] payload;
    
    protected RawPayload(final byte[] payload) {
        this.payload = payload;
    }
    
    @Override
    public boolean isJsonStructure() {
        return false;
    }

    @Override
    public boolean isRawPayload() {
        return true;
    }

    @Override
    public JsonStructure getJsonStructure() throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public byte[] getRawPayload() throws JsonLdError {
        return payload;
    }
}
