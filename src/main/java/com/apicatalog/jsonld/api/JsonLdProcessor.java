package com.apicatalog.jsonld.api;

import java.net.URI;
import java.util.Collection;

import javax.json.JsonArray;
import javax.json.JsonObject;

import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.rdf.RdfDataset;

/**
 * The {@link JsonLdProcessor} interface is the high-level programming structure
 * that developers use to access the JSON-LD transformation methods.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#the-jsonldprocessor-interface">JsonLdProcessor
 *      Specification</a>
 *
 */
public interface JsonLdProcessor {


    JsonObject compact(URI documentUri, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

    JsonObject compact(RemoteDocument remoteDocument, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

    JsonObject compact(JsonObject object, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

    JsonObject compact(Collection<JsonObject> objects, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

    
    

    JsonArray expand(URI documentUrl, JsonLdOptions options) throws JsonLdError;

    JsonArray expand(RemoteDocument remoteDocument, JsonLdOptions options) throws JsonLdError;

    JsonArray expand(JsonObject object, JsonLdOptions options) throws JsonLdError;
    
    JsonArray expand(Collection<JsonObject> objects, JsonLdOptions options) throws JsonLdError;

    
    
    
    JsonObject flatten(JsonLdInput input) throws JsonLdError;

    JsonObject flatten(JsonLdInput input, JsonLdContext context) throws JsonLdError;

    JsonObject flatten(JsonLdInput input, JsonLdOptions options) throws JsonLdError;

    JsonObject flatten(JsonLdInput input, JsonLdContext context, JsonLdOptions options) throws JsonLdError;

    
    
    JsonArray fromRdf(RdfDataset input) throws JsonLdError;

    JsonArray fromRdf(RdfDataset input, JsonLdOptions options) throws JsonLdError;

    
    
    RdfDataset toRdf(JsonLdInput input) throws JsonLdError;

    RdfDataset toRdf(JsonLdInput input, JsonLdOptions options) throws JsonLdError;
}
