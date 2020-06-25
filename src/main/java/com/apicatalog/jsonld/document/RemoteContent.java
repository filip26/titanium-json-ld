package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.io.Reader;
import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;

/**
 * {@link RemoteDocument} content holder.
 *  
 * This can either be the raw payload or already parsed JSON document.
 */
public interface RemoteContent {

    /**
     * Returns <code>true</code> if the content is {@link JsonStructure}
     * and {@link #getJsonStructure()} is not {@link Optional#empty()}.
     * 
     * @return <code>true</code> if the content is parsed JSON
     */
    boolean isJsonStructure();
    
    /**
     * Returns <code>true</code> if the content is raw payload
     * and {@link #getRawPayload()} is not {@link Optional#empty()}.
     * 
     * @return <code>true</code> if the content is raw payload
     */
    boolean isRawPayload();
    
    /**
     * Get the document content as {@link JsonStructure}.
     * 
     * @return {@link JsonStructure} or {@link Optional#empty()) if document content is not parsed
     * @throws JsonLdError
     */
    Optional<JsonStructure> getJsonStructure() throws JsonLdError;

    
    /**
     * Get the document content as byte array.
     * 
     * @return document content as byte array or {@link Optional#empty()} if raw payload is not available.
     * @throws JsonLdError
     */
    Optional<byte[]> getRawPayload() throws JsonLdError;
   
    /**
     * Create a new document from byte array.
     * 
     * @param payload representing unparsed raw  content
     * @return {@link RemoteContent} representing unparsed content
     */
    public static RemoteContent of(final byte[] payload) {
        
        if (payload == null) {
            throw new IllegalArgumentException("The provided payload is null.");
        }
        
        return new RemoteRawContent(payload);
    }

    /**
     * Create a new document from {@link JsonStructure}.
     * 
     * @param structure representing parsed JSON content
     * @return {@link RemoteContent} representing JSON content
     */
    public static RemoteContent of(final JsonStructure structure) {
        
        if (structure == null) {
            throw new IllegalArgumentException("The provided JSON structure is null.");
        }
        
        return new RemoteJsonContent(structure);
    }
    
    public static RemoteContent parseJson(final InputStream is)  throws JsonLdError {
        
        if (is == null) {
            throw new IllegalArgumentException("The provided InputStream is null.");
        }
        
        return RemoteJsonContent.parse(is);
    }
    
    public static RemoteContent parseJson(final Reader reader)  throws JsonLdError {
        
        if (reader == null) {
            throw new IllegalArgumentException("The provided Reader is null.");
        }
        
        return RemoteJsonContent.parse(reader);        
    }

}
