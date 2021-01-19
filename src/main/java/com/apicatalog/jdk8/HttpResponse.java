package com.apicatalog.jdk8;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.util.Collections;
import java.util.List;

public class HttpResponse implements Closeable {

    private int statusCode;
    private HttpURLConnection conn;

    public HttpResponse(int statusCode, HttpURLConnection conn) {
        this.statusCode = statusCode;
        this.conn = conn;
    }

    public int statusCode() {
        return statusCode;
    }

    public List<String> headers(String h) {
        List<String> headerValues = conn.getHeaderFields().get(h);
        return headerValues == null ? Collections.emptyList() : headerValues;
    }

    public InputStream body() throws IOException {
        return conn.getInputStream();
    }

    @Override
    public void close() {
        conn.disconnect();
    }
}
