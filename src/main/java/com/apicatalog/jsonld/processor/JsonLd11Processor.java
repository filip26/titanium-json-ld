package com.apicatalog.jsonld.processor;

import java.net.URI;
import java.util.Collection;

import javax.json.JsonArray;
import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdContext;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdInput;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.rdf.RdfDataset;

public final class JsonLd11Processor implements JsonLdProcessor {

    @Override
    public JsonObject compact(final URI documentUri, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonObject compact(final RemoteDocument remoteDocument, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonObject compact(final JsonObject object, final JsonLdContext context, final JsonLdOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonObject compact(final Collection<JsonObject> objects, final JsonLdContext context, final JsonLdOptions options)  throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonArray expand(final URI documentUrl, final JsonLdOptions options) throws JsonLdError {
        return ExpansionProcessor.expand(documentUrl, options);
    }

    @Override
    public JsonArray expand(final RemoteDocument remoteDocument, final JsonLdOptions options) throws JsonLdError {
        return ExpansionProcessor.expand(remoteDocument, options);
    }

    @Override
    public JsonArray expand(final JsonObject object, final JsonLdOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonArray expand(final Collection<JsonObject> objects, final JsonLdOptions options) throws JsonLdError {
        throw new UnsupportedOperationException();
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
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<JsonObject> fromRdf(RdfDataset input) {
        return fromRdf(input, new JsonLdOptions());
    }

    @Override
    public Collection<JsonObject> fromRdf(RdfDataset input, JsonLdOptions options) {
        throw new UnsupportedOperationException();
    }

    @Override
    public RdfDataset toRdf(JsonLdInput input) {
        return toRdf(input, new JsonLdOptions());
    }

    @Override
    public RdfDataset toRdf(JsonLdInput input, JsonLdOptions options) {
        throw new UnsupportedOperationException();
    }
}
