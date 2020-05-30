package com.apicatalog.jsonld.api.builder;

import java.io.Writer;

import javax.json.JsonArray;

import com.apicatalog.jsonld.api.JsonLdOptions;

public final class ExpansionApi {

    
    public ExpansionApi options(JsonLdOptions options) {
        return this;
    }
    
    public JsonArray get() {
        return null;
    }
    
    public void toJson(Writer writer) {
        throw new UnsupportedOperationException();
    }
    
}
