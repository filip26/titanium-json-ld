package com.apicatalog.jdk8;

import okhttp3.Response;

import java.io.Closeable;
import java.io.InputStream;
import java.util.List;

public class HttpResponse implements Closeable {

    private Response response;

    public HttpResponse(Response response) {
        this.response = response;
    }

    public int statusCode() {
        return response.code();
    }

    public List<String> headers(String h) {
        return response.headers(h);
    }

    public InputStream body() {
        return response.body().byteStream();
    }

    @Override
    public void close() {
        response.close();
    }
}
