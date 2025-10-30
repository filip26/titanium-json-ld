package com.apicatalog.jsonld.http;

import java.net.URI;
import java.time.Duration;

import com.apicatalog.jsonld.JsonLdException;

public interface HttpClient {

    HttpResponse send(URI targetUri, String requestProfile) throws JsonLdException;
    
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
