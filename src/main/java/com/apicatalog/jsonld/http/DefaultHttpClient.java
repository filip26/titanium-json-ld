/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.http;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.time.Duration;
import java.util.Collection;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.LoaderClient;

public final class DefaultHttpClient implements LoaderClient {

    @Deprecated
    private static final java.net.http.HttpClient CLIENT = HttpClient.newBuilder().followRedirects(Redirect.NEVER).build();

    @Deprecated
    private static final DefaultHttpClient INSTANCE = new DefaultHttpClient(CLIENT);

    private final HttpClient httpClient;
    private Duration timeout;

    public DefaultHttpClient(final HttpClient httpClient) {
        this.httpClient = httpClient;
        this.timeout = null;
    }

    public LoaderClient.Response send(URI targetUri, String requestProfile) throws JsonLdException {

        HttpRequest.Builder request = HttpRequest.newBuilder()
                .GET()
                .uri(targetUri)
                .header("Accept", requestProfile);

        if (timeout != null && !timeout.isNegative() && !timeout.isZero()) {
            request = request.timeout(timeout);
        }

        try {
            return new HttpResponseImpl(httpClient.send(request.build(), BodyHandlers.ofInputStream()));

        } catch (InterruptedException e) {

            Thread.currentThread().interrupt();
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);

        } catch (IOException e) {

            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    @Deprecated
    public static final LoaderClient defaultInstance() {
        return INSTANCE;
    }

    @Override
    public LoaderClient timeout(Duration timeout) {
        this.timeout = timeout;
        return this;
    }

    public static class HttpResponseImpl implements LoaderClient.Response {

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
        public void close() {
            /* unused */ }
    }
}
