package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.net.http.HttpTimeoutException;
import java.time.Duration;
import java.util.Collection;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;

final class DefaultHttpClient implements HttpLoaderClient {

    private final HttpClient httpClient;
    private Duration timeout;

    DefaultHttpClient(final HttpClient httpClient) {
        this.httpClient = httpClient;
        this.timeout = null;
    }

    public HttpLoaderClient.Response send(URI targetUri, Collection<String> requestProfiles) throws JsonLdException {

        HttpRequest.Builder request = HttpRequest.newBuilder()
                .GET()
                .uri(targetUri)
                .header("Accept", HttpLoader.acceptHeader(requestProfiles));

        if (timeout != null && !timeout.isNegative() && !timeout.isZero()) {
            request = request.timeout(timeout);
        }

        try {
            return new HttpResponseImpl(httpClient.send(request.build(), BodyHandlers.ofInputStream()));

        } catch (InterruptedException e) {

            Thread.currentThread().interrupt();
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);

        } catch (HttpTimeoutException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_TIMEOUT, e);

        } catch (IOException e) {

            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    @Override
    public HttpLoaderClient timeout(Duration timeout) {
        this.timeout = timeout;
        return this;
    }

    private static class HttpResponseImpl implements HttpLoaderClient.Response {

        private final HttpResponse<InputStream> response;

        HttpResponseImpl(HttpResponse<InputStream> response) {
            this.response = response;
        }

        @Override
        public boolean isRedirect() {
            return response.statusCode() == 301
                    || response.statusCode() == 302
                    || response.statusCode() == 303
                    || response.statusCode() == 307;
        }

        @Override
        public boolean isSuccess() {
            return response.statusCode() == 200;
        }

        @Override
        public InputStream body() {
            return response.body();
        }

        @Override
        public Collection<String> links() {
            return response.headers().map().get("link");
        }

        @Override
        public Optional<String> contentType() {
            return response.headers().firstValue("content-type");
        }

        @Override
        public Optional<String> location() {
            return response.headers().firstValue("location");
        }

        @Override
        public int statusCode() {
            return response.statusCode();
        }

        @Override
        public void close() {
            /* unused */ }

    }
}