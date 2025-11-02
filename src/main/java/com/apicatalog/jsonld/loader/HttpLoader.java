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
import java.net.http.HttpClient;
import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdProfile;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.web.link.Link;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

public class HttpLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(HttpLoader.class.getName());

    public static final int MAX_REDIRECTIONS = 10;

    private static final String PLUS_JSON = "+json";

    private final int maxRedirections;

    private final HttpLoaderClient client;

    private final NodeParser reader;

    HttpLoader(HttpLoaderClient httpClient, NodeParser reader, int maxRedirections) {
        this.client = httpClient;
        this.maxRedirections = maxRedirections;
        this.reader = reader;
    }

    public static HttpLoader newLoader(final HttpClient httpClient, NodeParser reader) {
        return new HttpLoader(new DefaultHttpClient(httpClient), reader, MAX_REDIRECTIONS);
    }

    public static HttpLoader newLoader(final HttpClient httpClient, NodeParser reader, int maxRedirections) {
        return new HttpLoader(new DefaultHttpClient(httpClient), reader, maxRedirections);
    }

    @Override
    public Document loadDocument(final URI uri, final Options options) throws JsonLdException {

        try {
            URI targetUri = uri;

            MediaType contentType = null;

            URI contextUri = null;

            for (int redirection = 0; redirection < maxRedirections; redirection++) {

                // 2.
                try (HttpLoaderClient.Response response = client.send(targetUri, options.requestProfile())) {

                    // 3.
                    if (response.isRedirect()) {

                        final Optional<String> location = response.location();

                        if (location.isPresent()) {
                            targetUri = UriResolver.resolveAsUri(targetUri, location.get());
                            continue;
                        }

                        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                    }

                    if (!response.isSuccess()) {
                        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "]");
                    }

                    contentType = response.contentType()
                            .map(MediaType::of)
                            .orElse(null);

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
                                    .filter(l -> l.relations().contains(JsonLdProfile.CONTEXT))
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

    private final Document read(
            final MediaType type,
            final URI targetUri,
            final URI contextUrl,
            final HttpLoaderClient.Response response) throws JsonLdException, IOException {

        try (final var is = response.body()) {

            final var remoteContent = reader.read(is);

            final var remoteDocument = RemoteDocument.of(type, remoteContent);

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

//    /**
//     * Set fallback content-type used when received content-type is not supported.
//     * e.g. <code>setFallbackContentType(MediaType.JSON_LD)</code>
//     *
//     * @param fallbackContentType a content type that overrides unsupported received
//     *                            content-type
//     * @return {@link DefaulLoader} instance
//     * @since 1.4.0
//     */
//    @Deprecated
//    public DefaulLoader fallbackContentType(MediaType fallbackContentType) {
////FIXME        reader.setFallbackContentType(fallbackContentType);
//        return this;
//    }

    /**
     * Sets a timeout for a request. If the response is not received within the
     * specified timeout then an {@link JsonLdException}, code =
     * <code>LOADING_DOCUMENT_TIMEOUT</code> is thrown.
     *
     * @param timeout to set or <code>null</code> for no timeout
     * @return {@link HttpLoader} instance
     * @since 1.6.1
     */
    public HttpLoader timeout(Duration timeout) {
        client.timeout(timeout);
        return this;
    }

    public static final String acceptHeader() {
        return acceptHeader(null);
    }

    public static final String acceptHeader(final Collection<String> profiles) {
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

}
