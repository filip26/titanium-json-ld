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
import com.apicatalog.rdf.RdfDataset;

public final class JsonDocument implements Document {

    private static final String PLUS_JSON = "+json";
        
    private final MediaType contentType;
    private final JsonStructure structure;
    private final String profile;

    private URI documentUrl;
    private URI contentUrl;
    
    private JsonDocument(final MediaType type, final String profile, final JsonStructure structue) {
        this.contentType = type;
        this.profile = profile;
        this.structure = structue;
    }

    /**
     * Create a new document from {@link JsonStructure}. Sets {@link MediaType#JSON} as the content type.
     *
     * @param structure representing parsed JSON content
     * @return {@link Document} representing JSON content
     */
    public static JsonDocument of(final JsonStructure structure) {
        return of(MediaType.JSON, structure);
    }
    
    /**
     * Create a new document from {@link JsonStructure}.
     *
     * @param contentType reflecting the provided {@link JsonStructure}, e.g. {@link MediaType#JSON_LD}, any JSON based media type is allowed
     * @param structure representing parsed JSON content
     * @return {@link Document} representing JSON content 
     */
    public static JsonDocument of(final MediaType contentType, final JsonStructure structure) {

        if (contentType == null) {
            throw new IllegalArgumentException("The provided JSON type is null.");
        }

        assertContentType(contentType);
        
        if (structure == null) {
            throw new IllegalArgumentException("The provided JSON structure is null.");
        }
        
        final String profile = contentType.parameters().firstValue("profile").orElse(null);
        
        return new JsonDocument(contentType, profile, structure);
    }
    
    public static final JsonDocument of(final MediaType type, final InputStream is)  throws JsonLdError {
    
        assertContentType(type);
        
        try (final JsonParser parser = Json.createParser(is)) {

            return doParse(type, parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static final JsonDocument of(final MediaType type, final Reader reader)  throws JsonLdError {
        
        assertContentType(type);
        
        try (final JsonParser parser = Json.createParser(reader)) {

            return doParse(type, parser);
            
        } catch (JsonException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }
    
    private static final JsonDocument doParse(final MediaType type, final JsonParser parser) throws JsonLdError {
        
        if (!parser.hasNext()) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Nothing to read. Provided document is empty.");
        }

        parser.next();

        final JsonValue root = parser.getValue();

        final String profile =type.parameters().firstValue("profile").orElse(null);
            
        if (JsonUtils.isArray(root)) {
            return new JsonDocument(type, profile, root.asJsonArray());
        }

        if (JsonUtils.isObject(root)) {
            return new JsonDocument(type, profile, root.asJsonObject());
        }

        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "JSON document's top level element must be JSON array or object.");
    }
    
    public static final boolean accepts(final MediaType contentType) {
        return contentType != null &&
                (MediaType.JSON_LD.match(contentType)
                || MediaType.JSON.match(contentType)
                || contentType.subtype().toLowerCase().endsWith(PLUS_JSON));        
    }
    
    private static final void assertContentType(final MediaType contentType) {
        if (!accepts(contentType)) {
            throw new IllegalArgumentException(
                    "Unsupported media type '" + contentType 
                    + "'. Supported content types are [" 
                    + MediaType.JSON_LD + ", " 
                    + MediaType.JSON  + ", +json]");
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

    @Override
    public Optional<RdfDataset> getRdfContent() {
        return Optional.empty();
    }
}
