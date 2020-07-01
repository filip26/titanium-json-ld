package com.apicatalog.jsonld.document;

import java.net.URI;
import java.util.Optional;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;

/**
 * A document that can be processed by the processor. 
 * 
 * This can either be {@link JsonStructure}, representing JSON-LD or JSON document, 
 * or {@link RdfDataset}
 * 
 * Implemented by {@link JsonDocument}, {@link RdfDocument}, and provided by {@link DocumentParser}.
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
    public default  Optional<JsonStructure> getJsonContent() {
        return Optional.empty();
    }

    /**
     * Get the document content as parsed {@link RdfDataset}.
     * 
     * @return {@link RdfDataset} or {@link Optional#empty()} if document content is not in <code>application/n-quads</code> representation
     */
    public default Optional<RdfDataset> getRdfContent() {
        return Optional.empty();
    }

}
