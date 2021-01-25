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
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.loader.HttpClient;
import com.apicatalog.jsonld.loader.HttpResponse;
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

    private static final HttpClient CLIENT = new HttpClient();

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

    public static DocumentLoader defaultInstance() {
        return INSTANCE;
    }

    private Document loadDocument(URI uri, URI targetUri, DocumentLoaderOptions options, int redirection) throws JsonLdError {

        if (maxRedirections > 0 && redirection >= maxRedirections) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
        }

        MediaType contentType = null;
        URI contextUri = null;

        // 2.
        try (HttpResponse response = httpClient.get(targetUri.toURL(), getAcceptHeader(options.getRequestProfile()))) {

            // 3.
            if (response.statusCode() == 301
                    || response.statusCode() == 302
                    || response.statusCode() == 303
                    || response.statusCode() == 307
            ) {
                final Optional<String> location = firstValue(response.headers("Location"));

                if (location.isPresent()) {
                    return loadDocument(
                            uri,
                            URI.create(UriResolver.resolve(targetUri, location.get())),
                            options,
                            ++redirection
                    );
                } else {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                }
            }

            if (response.statusCode() != 200) {
                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "]");
            }

            final Optional<String> contentTypeValue = firstValue(response.headers("Content-Type"));

            if (contentTypeValue.isPresent()) {
                contentType = MediaType.of(contentTypeValue.get());
            }

            final List<String> linkValues = response.headers("Link");

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
                        return loadDocument(uri, alternate.get().target(), options, ++redirection);
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

            if (contentType == null) {
                LOGGER.log(Level.WARNING, "GET on URL [{0}] does not return content-type header. Trying application/json.", uri);
                contentType = MediaType.JSON;
            }
            return createDocument(contentType, targetUri, contextUri, response);
        } catch(IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    @Override
    public Document loadDocument(final URI uri, final DocumentLoaderOptions options) throws JsonLdError {
        return loadDocument(uri, uri, options, 0);
    }

    public static String getAcceptHeader() {
        return getAcceptHeader(null);
    }

    public static String getAcceptHeader(final Collection<String> profiles) {
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

    public static Document createDocument(
            final MediaType type,
            final URI targetUri,
            final URI contextUrl,
            final HttpResponse response) throws JsonLdError, IOException {
        try (final InputStream is = response.body()) {

            final Document remoteDocument = DocumentParser.parse(type, is);

            remoteDocument.setDocumentUrl(targetUri);

            remoteDocument.setContextUrl(contextUrl);

            return remoteDocument;
        }
    }

    private static <T> Optional<T> firstValue(List<T> l) {
        if (l == null || l.isEmpty()) {
            return Optional.empty();
        } else {
            return Optional.of(l.get(0));
        }
    }
}
