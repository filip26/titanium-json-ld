package com.apicatalog.jsonld.http;

import java.net.URI;
import java.time.Duration;

import com.apicatalog.jsonld.JsonLdError;

public interface HttpClient {

    HttpResponse send(URI targetUri, String requestProfile) throws JsonLdError;
    
    /**
     * Set read timeout
     * 
     * @param timeout to set or <code>null</code> for no timeout
     */
    void setTimeout(Duration timeout);

}
