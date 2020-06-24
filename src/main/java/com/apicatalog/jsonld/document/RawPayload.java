package com.apicatalog.jsonld.document;

import java.util.Optional;

import javax.json.JsonStructure;

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
    public Optional<JsonStructure> getJsonStructure() {
        return Optional.empty();
    }

    @Override
    public Optional<byte[]> getRawPayload() {
        return Optional.of(payload);
    }
}
