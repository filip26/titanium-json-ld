package com.apicatalog.jdk8;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpClient {

    public static final int connectTimeoutMillis = 5000;
    public static final int readTimeoutMillis = 5000;

    public HttpResponse get(URL url, String accept) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setConnectTimeout(connectTimeoutMillis);
        conn.setReadTimeout(readTimeoutMillis);
        conn.setInstanceFollowRedirects(false);
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Accept", accept);
        int statusCode = conn.getResponseCode();
        return new HttpResponse(statusCode, conn);
    }
}
