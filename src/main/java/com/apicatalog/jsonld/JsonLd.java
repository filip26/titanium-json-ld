package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;
import com.apicatalog.jsonld.api.builder.FlatteningApi;

public final class JsonLd {

    private JsonLd() {
    }
    
    public static final ExpansionApi expand(String documentUri) {
        return expand(URI.create(documentUri));
    }

    public static final ExpansionApi expand(URI documentUri) {
        return new ExpansionApi(documentUri);
    }
    
    public static final CompactionApi compact(String documentUri, String contextUri) {
        return compact(documentUri, URI.create(contextUri));
    }
    
    public static final CompactionApi compact(String documentUri, URI contextUri) {
        return compact(URI.create(documentUri), contextUri);
    }
    
    public static final CompactionApi compact(URI documentUri, URI contextUri) {
        return new CompactionApi(documentUri, contextUri);
    }

    public static final CompactionApi compact(String documentUri, JsonStructure context) {
        return compact(URI.create(documentUri), context);
    }

    public static final CompactionApi compact(URI documentUri, JsonStructure context) {
        return new CompactionApi(documentUri, context);
    }

    public static final FlatteningApi flatten(String documentUri, String contextUri) {
        return flatten(documentUri, URI.create(contextUri));
    }
    
    public static final FlatteningApi flatten(String documentUri, URI contextUri) {
        return flatten(URI.create(documentUri), contextUri);
    }
    
    public static final FlatteningApi flatten(URI documentUri, URI contextUri) {
        return new FlatteningApi(documentUri, contextUri);
    }

    public static final FlatteningApi flatten(String documentUri, JsonStructure context) {
        return flatten(URI.create(documentUri), context);
    }

    public static final FlatteningApi flatten(URI documentUri, JsonStructure context) {
        return new FlatteningApi(documentUri, context);
    }
    
    public static final JsonLdProcessor createProcessor() {
        throw new UnsupportedOperationException();
    }
}
