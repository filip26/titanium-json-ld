package com.apicatalog.jsonld.http;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class DefaultHttpClient implements HttpClient {

    private static final OkHttpClient CLIENT = new OkHttpClient.Builder()
            .followRedirects(false)
            .followSslRedirects(false)
            .build();

    static final DefaultHttpClient INSTANCE = new DefaultHttpClient(CLIENT);

    private OkHttpClient okHttpClient;

    public DefaultHttpClient() {
        this(CLIENT);
    }

    public DefaultHttpClient(OkHttpClient okayHttpClient) {
        this.okHttpClient = okayHttpClient;
    }

    @Override
    public HttpResponse send(URI targetUri, String requestProfile) throws JsonLdError {

        try {
            final Request request = new Request.Builder()
                    .header("Accept", requestProfile)
                    .url(targetUri.toURL())
                    .build();

            return new HttpResponseImpl(okHttpClient.newCall(request).execute());

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static final HttpClient defaultInstance() {
        return INSTANCE;
    }

    static class HttpResponseImpl implements HttpResponse {

        private Response response;

        public HttpResponseImpl(Response response) {
            this.response = response;
        }

        public int statusCode() {
            return response.code();
        }

        @Override
        public InputStream body() {
            return response.body().byteStream();
        }

        @Override
        public void close() {
            response.close();
        }

        @Override
        public Collection<String> links() {
            return response.headers("link");
        }

        @Override
        public Optional<String> contentType() {
            return firstValue(response.headers("content-type"));
        }

        @Override
        public Optional<String> location() {
            return firstValue(response.headers("location"));
        }

        private static <T> Optional<T> firstValue(List<T> l) {
            if (l == null || l.isEmpty()) {
                return Optional.empty();
            } else {
                return Optional.of(l.get(0));
            }
        }
    }
}
