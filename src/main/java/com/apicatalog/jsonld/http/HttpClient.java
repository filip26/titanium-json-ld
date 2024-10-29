package com.apicatalog.jsonld.http;

import java.net.URI;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;

/**
 * Generic interface representing a simple HTTP client allowing to retrieve a
 * resource representing a document.
 */
public interface HttpClient {

    /**
     * Send a request to get a resource representing a document. Please note it's up
     * to an implementation of this interface which method, what headers to use to
     * retrieve a document identified by an {@link URI} and a profile.
     * 
     * @param targetUri      requested document {@link URI}
     * @param requestProfile requested document profile
     * @return {@link CompletableFuture} holding the document retrieval processing
     *         state
     *         
     * @since 2.0.0
     */
    CompletableFuture<HttpResponse> send(URI targetUri, String requestProfile);

    /**
     * Configure read timeout
     * 
     * @param timeout to set or <code>null</code> for no timeout
     * 
     * @return {@link HttpClient} instance,
     * 
     * @since 1.4.0
     */
    default HttpClient timeout(Duration timeout) {
        throw new UnsupportedOperationException();
    }
}
