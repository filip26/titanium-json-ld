package com.apicatalog.jsonld.document;

import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;

/**
 * A document content holder. 
 * This can either be the raw payload or already parsed <code>JSON</code> document.
 */
public interface Document {

    boolean isJsonStructure();
    boolean isRawPayload();
    
    /**
     * Get the document content as {@link JsonStructure}.
     * 
     * @return {@link JsonStructure} or <code>Optional.empty()</code> if document content is not parsed
     * @throws JsonLdError
     */
    Optional<JsonStructure> getJsonStructure() throws JsonLdError;

    
    /**
     * Get the document content as byte array.
     * 
     * @return document content as byte array or <code>Optional.empty()</code> if raw payload is not available.
     * @throws JsonLdError
     */
    Optional<byte[]> getRawPayload() throws JsonLdError;
   
    /**
     * Create a new document from byte array.
     * 
     * @param payload representing unparsed raw  content
     * @return {@link Document} representing unparsed content
     */
    public static Document of(final byte[] payload) {
        
        if (payload == null) {
            throw new IllegalArgumentException("The provided payload is null.");
        }
        
        return new RawPayload(payload);
    }

    /**
     * Create a new document from {@link JsonStructure}.
     * 
     * @param structure representing parsed <code>JSON</code> content
     * @return {@link Document} representing <code>JSON</code> content
     */
    public static Document of(final JsonStructure structure) {
        
        if (structure == null) {
            throw new IllegalArgumentException("The provided JSON structure is null.");
        }
        
        return new JsonDocument(structure);
    }
}
