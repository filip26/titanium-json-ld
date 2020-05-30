package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;

public final class JsonLd {

    private JsonLd() {
    }
    
    public static final ExpansionApi expand(String documentUri) {
        return new ExpansionApi();
    }

    public static final ExpansionApi expand(URI documentUri) {
        return new ExpansionApi();
    }

    public static final CompactionApi compact(String documentUri, JsonStructure context) {
        return new CompactionApi();
    }

    public static CompactionApi compact(URI documentUri, JsonStructure context) {
        return new CompactionApi();
    }

    public static final JsonLdProcessor createProcessor() {
        throw new UnsupportedOperationException();
    }
}
