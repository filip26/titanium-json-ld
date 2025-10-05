package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.jsonld.http.HttpClient;

public class HttpLoader extends DefaultHttpLoader {

    private static final HttpLoader INSTANCE = new HttpLoader(DefaultHttpClient.defaultInstance());

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
