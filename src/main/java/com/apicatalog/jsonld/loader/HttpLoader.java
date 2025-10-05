package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.HttpClient;

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

    public HttpLoader(HttpClient httpClient, int maxRedirections) {
        super(httpClient, maxRedirections);
    }

    public static final DefaultHttpLoader defaultInstance() {
        return INSTANCE;
    }
}
