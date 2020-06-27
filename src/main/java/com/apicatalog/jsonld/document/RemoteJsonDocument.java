package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.json.JsonUtils;

final class RemoteJsonDocument implements RemoteDocument {

    private static final String PLUS_JSON = "+json";
        
    private final MediaType contentType;
    private final JsonStructure structure;
    private final String profile;

    private URI documentUrl;
    private URI contentUrl;
    
    private RemoteJsonDocument(final MediaType type, final String profile, final JsonStructure structue) {
        this.contentType = type;
        this.profile = profile;
        this.structure = structue;
    }

    public static final RemoteDocument of(final MediaType type, final JsonStructure structure) {

        if (type == null) {
            throw new IllegalArgumentException("Document content type must be null.");
        }
        
        final String profile = type.parameters().firstValue("profile").orElse(null);
        
        return new RemoteJsonDocument(type, profile, structure);
    }
    
    public static final RemoteDocument of(final MediaType type, final InputStream is)  throws JsonLdError {
        
        assertContentType(type);
        
        try (final JsonParser parser = Json.createParser(is)) {

            return doParse(type, parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static final RemoteDocument of(final MediaType type, final Reader reader)  throws JsonLdError {
        
        assertContentType(type);
        
        try (final JsonParser parser = Json.createParser(reader)) {

            return doParse(type, parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    private static final RemoteDocument doParse(final MediaType type, final JsonParser parser) throws JsonLdError {
        
        if (!parser.hasNext()) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Nothing to read. Provided document is empty.");
        }

        parser.next();

        final JsonValue root = parser.getValue();

        final String profile =type.parameters().firstValue("profile").orElse(null);
            
        if (JsonUtils.isArray(root)) {
            return new RemoteJsonDocument(type, profile, root.asJsonArray());
        }

        if (JsonUtils.isObject(root)) {
            return new RemoteJsonDocument(type, profile, root.asJsonObject());
        }

        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "JSON document's top level element must be JSON array or object.");
    }

    protected static final void assertContentType(MediaType contentType) throws JsonLdError {
        if (contentType == null ||
                (!MediaType.JSON_LD.match(contentType)
                && !MediaType.JSON.match(contentType)
                && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))  
                ) {
            
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, 
                                        "Unsupported media type '" + contentType 
                                        + "'. Supported content types are [" 
                                        + MediaType.JSON_LD + ", " 
                                        + MediaType.JSON  + ", "
                                        + PLUS_JSON
                                        + "]"
                                        );
        }
    }
    
    @Override
    public Optional<JsonStructure> getJsonContent() {
        return Optional.of(structure);
    }

    @Override
    public MediaType getContentType() {
        return contentType;
    }

    @Override
    public URI getContextUrl() {
        return contentUrl;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
        this.contentUrl = contextUrl;
    }

    @Override
    public URI getDocumentUrl() {
        return documentUrl;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    @Override
    public Optional<String> getProfile() {
        return Optional.ofNullable(profile);
    }
}
