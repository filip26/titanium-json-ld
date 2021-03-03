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
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.DocumentParser;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.jsonld.http.link.Link;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.jsonld.uri.UriResolver;

public class HttpLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(HttpLoader.class.getName());

    private static final HttpClient CLIENT = HttpClient.newBuilder().followRedirects(Redirect.NEVER).build();

    private static final HttpLoader INSTANCE = new HttpLoader(CLIENT);

    public static final int MAX_REDIRECTIONS = 10;

    private static final String PLUS_JSON = "+json";

    private final int maxRedirections;

    private final HttpClient httpClient;

    public HttpLoader(HttpClient httpClient) {
        this(httpClient, MAX_REDIRECTIONS);
    }

    public HttpLoader(HttpClient httpClient, int maxRedirections) {
        this.httpClient = httpClient;
        this.maxRedirections = maxRedirections;
    }

    public static final DocumentLoader defaultInstance() {
        return INSTANCE;
    }

    @Override
    public Document loadDocument(final URI uri, final DocumentLoaderOptions options) throws JsonLdError {

        try {
            int redirection = 0;

            boolean done = false;

            URI targetUri = uri;

            MediaType contentType = null;

            URI contextUri = null;

            HttpResponse<InputStream> response = null;

            while (!done) {

                // 2.
                HttpRequest request =
                                HttpRequest.newBuilder()
                                    .GET()
                                    .uri(targetUri)
                                    .header("Accept", getAcceptHeader(options.getRequestProfile()))
                                    .build();

                response = httpClient.send(request, BodyHandlers.ofInputStream());

                // 3.
                if (response.statusCode() == 301
                    || response.statusCode() == 302
                    || response.statusCode() == 303
                    || response.statusCode() == 307
                    ) {

                    final Optional<String> location = response.headers().firstValue("Location");

                    if (location.isPresent()) {
                        targetUri = URI.create(UriResolver.resolve(targetUri, location.get()));

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                    }


                    redirection++;

                    if (maxRedirections > 0 && redirection >= maxRedirections) {
                        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                    }

                    continue;
                }

                if (response.statusCode() != 200) {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "]");
                }

                final Optional<String> contentTypeValue = response.headers().firstValue("Content-Type");

                if (contentTypeValue.isPresent()) {
                    contentType = MediaType.of(contentTypeValue.get());
                }

                final List<String> linkValues = response.headers().map().get("link");

                if (linkValues != null && !linkValues.isEmpty()) {

                    // 4.
                    if (contentType == null
                            || (!MediaType.JSON.match(contentType)
                                    && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))
                            ) {

                        final URI baseUri = targetUri;

                        Optional<Link> alternate =
                                            linkValues.stream()
                                                .flatMap(l -> Link.of(l, baseUri).stream())
                                                .filter(l -> l.relations().contains("alternate")
                                                                && l.type().isPresent()
                                                                && MediaType.JSON_LD.match(l.type().get())
                                                        )
                                                .findFirst();

                        if (alternate.isPresent()) {

                            targetUri = alternate.get().target();

                            redirection++;

                            if (maxRedirections > 0 && redirection >= maxRedirections) {
                                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                            }

                            continue;
                        }
                    }

                    // 5.
                    if (contentType != null
                            && !MediaType.JSON_LD.match(contentType)
                            && (MediaType.JSON.match(contentType)
                                    || contentType.subtype().toLowerCase().endsWith(PLUS_JSON))
                            ) {

                        final URI baseUri = targetUri;

                        final List<Link> contextUris =
                                        linkValues.stream()
                                            .flatMap(l -> Link.of(l, baseUri).stream())
                                            .filter(l -> l.relations().contains(ProfileConstants.CONTEXT))
                                            .collect(Collectors.toList());

                        if (contextUris.size() > 1) {
                            throw new JsonLdError(JsonLdErrorCode.MULTIPLE_CONTEXT_LINK_HEADERS);

                        } else if (contextUris.size() == 1) {
                            contextUri = contextUris.get(0).target();
                        }
                    }
                }

                done = true;
            }

            if (contentType == null) {
                LOGGER.log(Level.WARNING, "GET on URL [{0}] does not return content-type header. Trying application/json.", uri);
                contentType = MediaType.JSON;
            }

            return createDocument(contentType, targetUri, contextUri, response);

        } catch (InterruptedException e) {

            Thread.currentThread().interrupt();
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);

        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    public static final String getAcceptHeader() {
        return getAcceptHeader(null);
    }

    public static final String getAcceptHeader(final Collection<String> profiles) {
        final StringBuilder builder = new StringBuilder();

        builder.append(MediaType.JSON_LD.toString());

        if (profiles != null && !profiles.isEmpty()) {
            builder.append(";profile=\"");
            builder.append(String.join(" ", profiles));
            builder.append("\"");
        }

        builder.append(',');
        builder.append(MediaType.JSON.toString());
        builder.append(";q=0.9,*/*;q=0.8");
        return builder.toString();
    }

    public static final Document createDocument(
                                        final MediaType type,
                                        final URI targetUri,
                                        final URI contextUrl,
                                        final HttpResponse<InputStream> response) throws JsonLdError, IOException {
        try (final InputStream is = response.body()) {

            final Document remoteDocument = DocumentParser.parse(type, is);

            remoteDocument.setDocumentUrl(targetUri);

            remoteDocument.setContextUrl(contextUrl);

            return remoteDocument;
        }
    }
}
