package com.apicatalog.jsonld.loader;

import java.time.Duration;

import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.HttpClient;
import com.apicatalog.jsonld.http.media.MediaType;

public class HttpLoader extends DefaultHttpLoader {

    private static final HttpLoader INSTANCE = new HttpLoader(DefaultHttpClient.defaultInstance());

    /**
     * @deprecated use <code>HttpLoader(com.apicatalog.jsonld.http.HttpClient httpClient)</code>
     *
     * @param httpClient
     */
    @Deprecated(since = "1.0.3")
    public HttpLoader(java.net.http.HttpClient httpClient) {
        this(httpClient, MAX_REDIRECTIONS);
    }

    /**
     * @deprecated use <code>HttpLoader(com.apicatalog.jsonld.http.HttpClient httpClient, int maxRedirection)</code>
     *
     * @param httpClient
     * @param maxRedirections
     */
    @Deprecated(since = "1.0.3")
    public HttpLoader(java.net.http.HttpClient httpClient, int maxRedirections) {
        this(new DefaultHttpClient(httpClient), maxRedirections);
    }

    public HttpLoader(HttpClient httpClient) {
        super(httpClient);
    }

    /**
     * @deprecated Use {@link Builder#maxRedirections(int)}.
     * 
     * @param httpClient
     * @param maxRedirections
     */
    @Deprecated(since="1.4.0")
    public HttpLoader(HttpClient httpClient, int maxRedirections) {
        super(httpClient, maxRedirections);
    }

    public static final DocumentLoader defaultInstance() {
        return INSTANCE;
    }    
    
    public static final Builder newInstance() {
        //TODO
        return null;
    }
    
    /**
     * 
     * @since 1.4.0
     */
    public class Builder {
        
        /**
         * Set fallback content-type used when received content-type is not supported.
         * e.g. <code>setFallbackContentType(MediaType.JSON_LD)</code>
         *
         * @param fallbackContentType a content type that overrides unsupported received
         *                            content-type
         * @return {@link DefaultHttpLoader} instance
         */        
        public Builder fallbackContentType(MediaType fallbackContentType) {
//            resolver.setFallbackContentType(fallbackContentType);
            return this;
        }

        /**
         * Set read timeout and create a new instance
         * 
         * @param timeount to set or <code>null</code> for no timeout
         * @return a new {@link DefaultHttpLoader} instance
         */
        public Builder timeount(Duration timeount) {
            //new DefaultHttpLoader(httpClient.timeout(timeount), maxRedirections, resolver);
            return this;
        }
        
        public Builder maxRedirections(int maxRedirections) {
            //TODO
            return this;
        }

        public DocumentLoader build() {
            //TODO
            return null;
        }
        
    }

}
