package com.apicatalog.jsonld.loader;

import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.net.URL;

public class HttpClient {

    private static final OkHttpClient DEFAULT_CLIENT = new OkHttpClient.Builder()
            .followRedirects(false)
            .followSslRedirects(false)
            .build();

    private OkHttpClient okHttpClient;

    public HttpClient() {
        this(DEFAULT_CLIENT);
    }

    public HttpClient(OkHttpClient okayHttpClient) {
        this.okHttpClient = okayHttpClient;
    }

    public HttpResponse get(URL url, String accept) throws IOException {
        Request request = new Request.Builder()
                .header("Accept", accept)
                .url(url)
                .build();

        return new HttpResponse(okHttpClient.newCall(request).execute());
    }
}
