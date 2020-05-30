package com.apicatalog.jsonld;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;

public final class JsonLd {

    private JsonLd() {
    }
    
    public static final ExpansionApi expand(String documentUri) {
        return new ExpansionApi();
    }

    public static final CompactionApi compact(String documentUri, String contextUri) {
        return new CompactionApi();
    }

    public static final JsonLdProcessor createProcessor() {
        throw new UnsupportedOperationException();
    }
    
}
