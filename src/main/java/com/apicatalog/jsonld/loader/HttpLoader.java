package com.apicatalog.jsonld.loader;

import com.apicatalog.jsonld.http.DefaultHttpClient;

public class HttpLoader extends DefaulLoader {

    private static final HttpLoader INSTANCE = new HttpLoader(DefaultHttpClient.defaultInstance());

    public HttpLoader(LoaderClient httpClient) {
        super(httpClient);
    }

    public HttpLoader(LoaderClient httpClient, int maxRedirections) {
        super(httpClient, maxRedirections);
    }

    @Deprecated
    public static final DefaulLoader defaultInstance() {
        return INSTANCE;
    }
}
