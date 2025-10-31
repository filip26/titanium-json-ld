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
import java.net.URI;
import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.TreeDocument;
import com.apicatalog.jsonld.http.HttpClient;
import com.apicatalog.jsonld.http.HttpResponse;
import com.apicatalog.jsonld.http.ProfileConstants;
import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.tree.io.jakarta.JakartaReader;
import com.apicatalog.web.link.Link;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

import jakarta.json.Json;

class DefaultHttpLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(DefaultHttpLoader.class.getName());

    public static final int MAX_REDIRECTIONS = 10;

    private static final String PLUS_JSON = "+json";

    private final int maxRedirections;

    private final HttpClient httpClient;

    private final NodeParser reader;

    public DefaultHttpLoader(HttpClient httpClient) {
        this(httpClient, MAX_REDIRECTIONS);
    }

    public DefaultHttpLoader(HttpClient httpClient, int maxRedirections) {
        this.httpClient = httpClient;
        this.maxRedirections = maxRedirections;
        this.reader = new JakartaReader(Json.createReaderFactory(Map.of()));
        // FIXME
        // new DocumentResolver();
    }

    @Override
    public Document loadDocument(final URI uri, final LoaderOptions options) throws JsonLdException {

        try {
            URI targetUri = uri;

            MediaType contentType = null;

            URI contextUri = null;

            for (int redirection = 0; redirection < maxRedirections; redirection++) {

                // 2.
                try (HttpResponse response = httpClient.send(targetUri, getAcceptHeader(options.getRequestProfile()))) {

                    // 3.
                    if (response.statusCode() == 301
                            || response.statusCode() == 302
                            || response.statusCode() == 303
                            || response.statusCode() == 307) {

                        final Optional<String> location = response.location();

                        if (location.isPresent()) {
                            targetUri = UriResolver.resolveAsUri(targetUri, location.get());
                            continue;
                        }

                        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                    }

                    if (response.statusCode() != 200) {
                        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "]");
                    }

                    final Optional<String> contentTypeValue = response.contentType();

                    if (contentTypeValue.isPresent()) {
                        contentType = MediaType.of(contentTypeValue.get());
                    }

                    final Collection<String> linkValues = response.links();

                    if (linkValues != null && !linkValues.isEmpty()) {

                        // 4.
                        if (contentType == null
                                || (!MediaType.JSON.match(contentType)
                                        && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))) {

                            final URI baseUri = targetUri;

                            Optional<Link> alternate = linkValues.stream()
                                    .flatMap(l -> Link.of(l, baseUri).stream())
                                    .filter(l -> l.relations().contains("alternate")
                                            && MediaType.JSON_LD.match(l.type()))
                                    .findFirst();

                            if (alternate.isPresent()) {

                                targetUri = alternate.get().target();
                                continue;
                            }
                        }

                        // 5.
                        if (contentType != null
                                && !MediaType.JSON_LD.match(contentType)
                                && (MediaType.JSON.match(contentType)
                                        || contentType.subtype().toLowerCase().endsWith(PLUS_JSON))) {

                            final URI baseUri = targetUri;

                            final List<Link> contextUris = linkValues.stream()
                                    .flatMap(l -> Link.of(l, baseUri).stream())
                                    .filter(l -> l.relations().contains(ProfileConstants.CONTEXT))
                                    .collect(Collectors.toList());

                            if (contextUris.size() > 1) {
                                throw new JsonLdException(JsonLdErrorCode.MULTIPLE_CONTEXT_LINK_HEADERS);

                            } else if (contextUris.size() == 1) {
                                contextUri = contextUris.get(0).target();
                            }
                        }
                    }

                    if (contentType == null) {
                        LOGGER.log(Level.WARNING, "GET on URL [{0}] does not return content-type header. Trying application/json.", uri);
                        contentType = MediaType.JSON;
                    }

                    return read(contentType, targetUri, contextUri, response);
                }
            }

            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");

        } catch (IOException e) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
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
        builder.append(";q=0.9,*/*;q=0.1");
        return builder.toString();
    }

    private final Document read(
            final MediaType type,
            final URI targetUri,
            final URI contextUrl,
            final HttpResponse response) throws JsonLdException, IOException {

        try (final var is = response.body()) {

            final var remoteContent = reader.read(is);

            final var remoteDocument = TreeDocument.of(type, remoteContent);

            remoteDocument.setDocumentUrl(targetUri);

            remoteDocument.setContextUrl(contextUrl);

            return remoteDocument;
            
        } catch (IOException e) {
            throw e;
            
        } catch (Exception e) {
            // FIXME!!!
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    /**
     * Set fallback content-type used when received content-type is not supported.
     * e.g. <code>setFallbackContentType(MediaType.JSON_LD)</code>
     *
     * @param fallbackContentType a content type that overrides unsupported received
     *                            content-type
     * @return {@link DefaultHttpLoader} instance
     * @since 1.4.0
     */
    public DefaultHttpLoader fallbackContentType(MediaType fallbackContentType) {
//FIXME        reader.setFallbackContentType(fallbackContentType);
        return this;
    }

    /**
     * Set read timeout
     *
     * @param timeout to set or <code>null</code> for no timeout
     * @return {@link DefaultHttpLoader} instance
     * @since 1.6.1
     */
    public DefaultHttpLoader timeout(Duration timeout) {
        httpClient.timeout(timeout);
        return this;
    }
}
