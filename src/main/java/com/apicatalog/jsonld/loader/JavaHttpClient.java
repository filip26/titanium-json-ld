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
import java.util.Map.Entry;
import java.util.Optional;

import com.apicatalog.jsonld.loader.HttpLoader.Client;

final class JavaHttpClient implements HttpLoader.Client {

    private final HttpClient httpClient;

    private Duration timeout;
    private Collection<Entry<String, String>> headers;

    JavaHttpClient(final HttpClient httpClient) {
        this.httpClient = httpClient;
        this.timeout = null;
        this.headers = null;
    }

    public HttpLoader.Response send(URI targetUri, Collection<String> requestProfiles) throws LoaderException {

        HttpRequest.Builder request = HttpRequest.newBuilder()
                .GET()
                .uri(targetUri)
                .header("Accept", HttpLoader.acceptHeader(requestProfiles));

        if (headers != null && !headers.isEmpty()) {
            headers.forEach(h -> request.header(h.getKey(), h.getValue()));
        }

        if (timeout != null && !timeout.isNegative() && !timeout.isZero()) {
            request.timeout(timeout);
        }

        try {
            return new HttpResponseImpl(httpClient.send(request.build(), BodyHandlers.ofInputStream()));

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new LoaderException(targetUri, e);

        } catch (HttpTimeoutException e) {
            throw new LoaderException(targetUri, e);

        } catch (IOException e) {
            throw new LoaderException(targetUri, e);
        }
    }

    @Override
    public Client timeout(Duration timeout) {
        this.timeout = timeout;
        return this;
    }

    @Override
    public Client headers(Collection<Entry<String, String>> headers) {
        this.headers = headers;
        return this;
    }

    private static class HttpResponseImpl implements HttpLoader.Response {

        private final HttpResponse<InputStream> response;

        HttpResponseImpl(HttpResponse<InputStream> response) {
            this.response = response;
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