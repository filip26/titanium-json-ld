package com.apicatalog.jsonld;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdInput;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.processor.ExpansionProcessor;
import com.apicatalog.rdf.RdfDataset;

final class JsonLd11Processor implements JsonLdProcessor {

    @Override
    public JsonObject compact(JsonLdInput input) throws JsonLdError {
        return compact(input, new JsonLdOptions());
    }

    @Override
    public JsonObject compact(JsonLdInput input, JsonLdContext context) throws JsonLdError {
        return compact(input, context, new JsonLdOptions());
    }

    @Override
    public JsonObject compact(JsonLdInput input, JsonLdOptions options) throws JsonLdError {
        return compact(input, null, options);
    }

    @Override
    public JsonObject compact(JsonLdInput input, JsonLdContext context, JsonLdOptions options) throws JsonLdError {
        // TODO
        return null;
    }

    @Override
    public JsonArray expand(final JsonStructure input, final JsonLdOptions options) throws JsonLdError {
        return ExpansionProcessor.expand(input, options);
    }

    @Override
    public JsonArray expand(final URI input, final JsonLdOptions options) throws JsonLdError {
        return ExpansionProcessor.expand(input, options);
    }

    @Override
    public JsonArray expand(RemoteDocument input, final JsonLdOptions options) throws JsonLdError {
        return ExpansionProcessor.expand(input, options);
    }

    @Override
    public JsonObject flatten(JsonLdInput input) {
        return flatten(input, null, new JsonLdOptions());
    }

    @Override
    public JsonObject flatten(JsonLdInput input, JsonLdContext context) {
        return flatten(input, context, new JsonLdOptions());
    }

    @Override
    public JsonObject flatten(JsonLdInput input, JsonLdOptions options) {
        return flatten(input, null, options);
    }

    @Override
    public JsonObject flatten(JsonLdInput input, JsonLdContext context, JsonLdOptions options) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Collection<JsonObject> fromRdf(RdfDataset input) {
        return fromRdf(input, new JsonLdOptions());
    }

    @Override
    public Collection<JsonObject> fromRdf(RdfDataset input, JsonLdOptions options) {
        // TODO Auto-generated method stub
        return Collections.emptyList();
    }

    @Override
    public RdfDataset toRdf(JsonLdInput input) {
        return toRdf(input, new JsonLdOptions());
    }

    @Override
    public RdfDataset toRdf(JsonLdInput input, JsonLdOptions options) {
        // TODO Auto-generated method stub
        return null;
    }

}
