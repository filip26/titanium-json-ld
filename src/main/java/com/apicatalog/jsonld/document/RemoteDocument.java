package com.apicatalog.jsonld.document;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;

/**
 * The {@link RemoteDocument} is used by a {@link LoadDocumentCallback} to
 * return information about a remote document or context.
 * 
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#remotedocument">RemoteDocument
 *      Specification</a>
 *
 */
public final class RemoteDocument {

    private MediaType contentType;
    private URI contextUrl;

    private RemoteContent content;

    private URI documentUrl;
    private String profile;

    public RemoteDocument(final RemoteContent document) {
        this(document, null);
    }

    public RemoteDocument(final RemoteContent content, final URI documentUrl) {
        this.content = content;
        this.documentUrl = documentUrl;
    }

    
    /**
     * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
     * of the loaded document, exclusive of any optional parameters.
     * 
     * @return <code>Content-Type</code> of the loaded document or <code>null</code> if not avaiable
     */
    public MediaType getContentType() {
        return contentType;
    }

    /**
     * The value of the HTTP Link header when profile attribute matches <code>http://www.w3.org/ns/json-ld#context</code>.
     * 
     * @return attached {@link URI} referencing document context or <code>null</code> 
     */
    public URI getContextUrl() {
        return contextUrl;
    }

    /**
     * The final {@link URI} of the loaded document.
     * 
     * @return {@link URI} of the loaded document or <code>null</code> if not available
     */
    public URI getDocumentUrl() {
        return documentUrl;
    }
    
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    /**
     * The value of any <code>profile</code> parameter retrieved as part of the
     * original {@link #getContentType()}.
     * 
     * @return document profile or <code>null</code>
     */
    public String getProfile() {
        return profile;
    }

    /**
     * The retrieved document. This can either be the raw payload or the already
     * parsed document.
     * 
     * @return parsed or raw document
     */
    public RemoteContent getContent() {
        return content;
    }

    public void setContent(RemoteContent content) {
        this.content = content;
    }   
    
    public void setContentType(MediaType contentType) {
        this.contentType = contentType;
    }
    
    public void setContextUrl(URI contextUrl) {
        this.contextUrl = contextUrl;
    }
    
    public void setProfile(String profile) {
        this.profile = profile;
    }

    /**
     * Create an empty remote document holding just the given content.
     * 
     * @param structure {@link JsonStructure} to set as a content
     * @return {@link RemoteDocument} 
     */
    public static RemoteDocument of(final JsonStructure structure) {
        if (structure == null) {
            throw new IllegalArgumentException("The JSON structure cannot be null.");
        }
        
        return new RemoteDocument(RemoteContent.of(structure));
    }
    
    /**
     * Create an empty remote document holding just the given raw payload.
     * 
     * @param payload <code>byte[]</code> to set as raw/unparsed content
     * @return {@link RemoteDocument} 
     */
    public static RemoteDocument of(final byte[] payload) {
        if (payload == null) {
            throw new IllegalArgumentException("The JSON structure cannot be null.");
        }

        return new RemoteDocument(RemoteContent.of(payload));
    }    
}
