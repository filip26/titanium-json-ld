package com.apicatalog.jsonld.document;

import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;

/**
 * A document that can be processed by the processor. 
 * 
 * This can either be {@link JsonStructure}, representing JSON-LD or JSON document, 
 * or {@link RdfDataset)
 *
 */
public interface Document {
    
    /**
     * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
     * of the loaded document, exclusive of any optional parameters.
     * 
     * @return <code>Content-Type</code> of the loaded document, never <code>null</code>
     */
    MediaType getContentType();
    
    /**
     * The value of the HTTP Link header when profile attribute matches <code>http://www.w3.org/ns/json-ld#context</code>.
     * 
     * @return attached {@link URI} referencing document context or <code>null</code> if not available 
     */
    URI getContextUrl();
    
    void setContextUrl(URI contextUrl);
    
    /**
     * The final {@link URI} of the loaded document.
     * 
     * @return {@link URI} of the loaded document or <code>null</code> if not available
     */
    URI getDocumentUrl();
    
    void setDocumentUrl(URI documentUrl);

    /**
     * The value of any <code>profile</code> parameter retrieved as part of the
     * original {@link #getContentType()}.
     * 
     * @return document profile or {@link Optional#empty()}
     */
    Optional<String> getProfile();
    
    /**
     * Get the document content as parsed {@link JsonStructure}.
     * 
     * @return {@link JsonStructure} or {@link Optional#empty()} if document content is not JSON based
     */
    Optional<JsonStructure> getJsonContent();

    /**
     * Get the document content as parsed {@link RdfDataset}.
     * 
     * @return {@link RdfDataset} or {@link Optional#empty()} if document content is not in <code>application/n-quads</code> representation
     */
    Optional<RdfDataset> getRdfContent(); 
    
    /**
     * Create a new document from {@link JsonStructure}. Sets {@link MediaType#JSON} as the content type.
     *
     * @param structure representing parsed JSON content
     * @return {@link DocumentContent} representing JSON content
     */
    public static Document of(final JsonStructure structure) {
        return of(MediaType.JSON, structure);
    }
    
    /**
     * Create a new document from {@link JsonStructure}.
     *
     * @param contentType reflecting the provided {@link JsonStructure}, e.g. {@link MediaType#JSON_LD}, any JSON based media type is allowed
     * @param structure representing parsed JSON content
     * @return {@link DocumentContent} representing JSON content 
     */
    public static Document of(final MediaType contentType, final JsonStructure structure) {

        if (contentType == null) {
            throw new IllegalArgumentException("The provided JSON type is null.");
        }
        if (structure == null) {
            throw new IllegalArgumentException("The provided JSON structure is null.");
        }
        
        return JsonDocument.of(contentType, structure);
    }

    /**
     * Create a new document.
     * 
     * @param contentType {@link MediaType} of the raw content, must not be <code>null</code>
     * @param inputStream providing unparsed raw content described by {{@link MediaType}
     * @return {@link DocumentContent} representing unparsed content
     */
    public static Document of(final MediaType contentType, final InputStream inputStream)  throws JsonLdError {
        
        if (inputStream == null) {
            throw new IllegalArgumentException("The provided content InputStream is null.");
        }
        
        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }
                
        return DocumentParser.parse(contentType, inputStream);
    }

    /**
     * Create a new document.
     * 
     * @param contentType {@link MediaType} of the raw content, must not be <code>null</code>
     * @param readed providing unparsed raw content described by {{@link MediaType}
     * @return {@link DocumentContent} representing unparsed content
     */
    public static Document of(final MediaType contentType, final Reader reader)  throws JsonLdError {
        
        if (reader == null) {
            throw new IllegalArgumentException("The provided content reader is null.");
        }

        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }
        
        return DocumentParser.parse(contentType, reader);
    }
}
