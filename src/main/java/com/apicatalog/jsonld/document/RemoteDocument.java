package com.apicatalog.jsonld.document;

import java.net.URI;

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
public class RemoteDocument {

    private MediaType contentType;
    private URI contextUrl;

    private Document document;

    private URI documentUrl;
    private String profile;

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
    public Document getDocument() {
        return document;
    }

    public void setDocument(Document document) {
        this.document = document;
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
}
