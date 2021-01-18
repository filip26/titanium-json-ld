package com.apicatalog.jdk8;

import okhttp3.OkHttpClient;
import okhttp3.Request;

import java.io.IOException;
import java.net.URL;

public class HttpClient {

    private OkHttpClient client = new OkHttpClient.Builder()
            .followRedirects(false)
            .followSslRedirects(false)
            .build();

    public HttpResponse get(URL url, String accept) throws IOException {
        Request request = new Request.Builder()
                .header("Accept", accept)
                .url(url)
                .build();

        return new HttpResponse(client.newCall(request).execute());
    }
}
