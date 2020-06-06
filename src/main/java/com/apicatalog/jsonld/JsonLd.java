package com.apicatalog.jsonld;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.api.builder.CompactionApi;
import com.apicatalog.jsonld.api.builder.ExpansionApi;
import com.apicatalog.jsonld.api.builder.FlatteningApi;
import com.apicatalog.jsonld.api.builder.FromRdfApi;
import com.apicatalog.jsonld.api.builder.ToRdfApi;
import com.apicatalog.rdf.RdfDataset;

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

    public static final FlatteningApi flatten(String documentUri) {
        return flatten(URI.create(documentUri));
    }
    
    public static final FlatteningApi flatten(URI documentUri) {
        return new FlatteningApi(documentUri);
    }

    public static final ToRdfApi toRdf(String documentUri) {
        return toRdf(URI.create(documentUri));
    }
    
    public static final ToRdfApi toRdf(URI documentUri) {
        return new ToRdfApi(documentUri);
    }

    public static final FromRdfApi fromRdf(RdfDataset dataset) {
        return new FromRdfApi(dataset);
    }
    
    public static final JsonLdProcessor createProcessor() {
        throw new UnsupportedOperationException();
    }
}
