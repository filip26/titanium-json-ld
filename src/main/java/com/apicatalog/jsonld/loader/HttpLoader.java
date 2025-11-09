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
import java.net.http.HttpClient.Redirect;
import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.lang.Terms;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.web.link.Link;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

/**
 * Implementation of a {@link DocumentLoader} that retrieves JSON-LD documents
 * over HTTP.
 * <p>
 * This loader follows the JSON-LD 1.1 specification for remote context and
 * document loading. It supports content negotiation, media type checking, link
 * headers (for alternate or context URLs), and redirection up to a configurable
 * limit.
 * </p>
 *
 * <p>
 * The loader uses a configurable {@link HttpLoaderClient} to send HTTP requests
 * and a {@link TreeParser} to parse the retrieved content into {@link Document}
 * instances.
 * </p>
 *
 * @see DocumentLoader
 * @see HttpLoaderClient
 * @see TreeParser
 */
public class HttpLoader implements DocumentLoader {

    private static final Logger LOGGER = Logger.getLogger(HttpLoader.class.getName());

    private static final String PLUS_JSON = "+json";

    /**
     * The default predicate used to determine whether a {@link MediaType}
     * represents a JSON-LD compatible content type.
     * <p>
     * This predicate returns {@code true} if the provided media type is:
     * <ul>
     * <li>{@code application/ld+json}</li>
     * <li>{@code application/json}</li>
     * <li>or any media type whose subtype ends with {@code +json}</li>
     * </ul>
     * A {@code null} input always results in {@code false}.
     * </p>
     *
     * <p>
     * Used by default when no custom predicate is supplied through
     * {@link HttpLoader#accept(java.util.function.Predicate)}.
     * </p>
     *
     * @since 2.0.0
     */
    public static final Predicate<MediaType> DEFAULT_JSON_LD_CONTENT = contentType -> contentType != null
            && (MediaType.JSON_LD.match(contentType)
                    || MediaType.JSON.match(contentType)
                    || contentType.subtype().toLowerCase().endsWith(PLUS_JSON));
    /**
     * Default vendor headers added to every HTTP request.
     * 
     * @see {@link HttpLoader#headers(Collection)}
     */
    public static final Collection<Entry<String, String>> VENDOR_HEADERS = List.of(
            Map.entry(
                    "User-Agent",
                    "titanium-json-ld/2 (+https://github.com/filip26/titanium-json-ld)"));

    /**
     * Default maximum number of allowed HTTP redirections.
     */
    public static final int MAX_REDIRECTIONS = 10;

    private final int maxRedirections;

    private final HttpLoaderClient client;

    private final TreeParser reader;

    private Predicate<MediaType> acceptContent;

    /**
     * Constructs a new {@link HttpLoader}.
     *
     * @param httpClient      the HTTP client used for network requests
     * @param reader          the parser for decoding JSON documents
     * @param maxRedirections the maximum number of HTTP redirects to follow
     */
    protected HttpLoader(HttpLoaderClient httpClient, TreeParser reader, int maxRedirections) {
        this.client = httpClient;
        this.maxRedirections = maxRedirections;
        this.reader = reader;
        this.acceptContent = DEFAULT_JSON_LD_CONTENT;
    }

    /**
     * Creates a new {@link HttpLoader} with a default {@link HttpClient}.
     *
     * @param parser the {@link TreeParser} used to parse retrieved content
     * @return a new configured {@link HttpLoader}
     */
    public static HttpLoader newLoader(final TreeParser parser) {
        return newLoader(
                HttpClient
                        .newBuilder()
                        .followRedirects(Redirect.NEVER)
                        .build(),
                parser)
                .headers(VENDOR_HEADERS);
    }

    /**
     * Creates a new {@link HttpLoader} using the given {@link HttpClient} and
     * parser.
     *
     * @param client the HTTP client to use for network calls
     * @param reader the JSON parser used to parse responses
     * @return a configured {@link HttpLoader} instance
     */
    public static HttpLoader newLoader(final HttpClient client, TreeParser reader) {
        return new HttpLoader(new NativeHttpClient(client), reader, MAX_REDIRECTIONS)
                .headers(VENDOR_HEADERS);
    }

    /**
     * Creates a new {@link HttpLoader} with a custom maximum number of
     * redirections.
     *
     * @param client          the HTTP client to use
     * @param reader          the parser to use for JSON parsing
     * @param maxRedirections the maximum number of redirects to follow
     * @return a configured {@link HttpLoader}
     */
    public static HttpLoader newLoader(final HttpClient client, TreeParser reader, int maxRedirections) {
        return new HttpLoader(new NativeHttpClient(client), reader, maxRedirections)
                .headers(VENDOR_HEADERS);
    }

    /**
     * Loads a remote JSON-LD document from the given URI according to the JSON-LD
     * specification.
     * <p>
     * This method handles:
     * <ul>
     * <li>Redirections (301, 302, 303, 307) up to {@link #maxRedirections}</li>
     * <li>Content type negotiation and validation</li>
     * <li>Processing of HTTP Link headers for alternate or context URLs</li>
     * </ul>
     * </p>
     *
     * @param url     the URI of the document to load
     * @param options loader options such as requested profiles
     * @return the loaded {@link Document}
     * @throws JsonLdException if the document cannot be loaded, parsed, or
     *                         validated
     */
    @Override
    public Document loadDocument(final URI url, final Options options) throws JsonLdException {

        try {
            URI targetUri = url;

            MediaType contentType = null;

            URI contextUri = null;

            for (int redirection = 0; redirection < maxRedirections; redirection++) {

                // 2.
                try (final var response = client.send(targetUri, options.requestProfile())) {

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

                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                    }

                    if (response.statusCode() != 200) {
                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "] for URL [" + url + "].");
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

                            final var alternate = linkValues.stream()
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
                                    .filter(l -> l.relations().contains(Terms.PROFILE_CONTEXT))
                                    .collect(Collectors.toList());

                            if (contextUris.size() > 1) {
                                throw new JsonLdException(ErrorCode.MULTIPLE_CONTEXT_LINK_HEADERS);

                            } else if (contextUris.size() == 1) {
                                contextUri = contextUris.get(0).target();
                            }
                        }
                    }

                    if (contentType == null) {
                        LOGGER.log(Level.WARNING, "GET on URL [{0}] does not return content-type header.", url);

                    } else if (!acceptContent.test(contentType)) {
                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Unsupported content-type '" + contentType + "'.");
                    }

                    return read(contentType, targetUri, contextUri, response);
                }
            }

            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");

        } catch (IOException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    private final Document read(
            final MediaType contentType,
            final URI targetUri,
            final URI contextUrl,
            final HttpLoaderClient.Response response) throws JsonLdException {

        try (final var is = response.body()) {

            final var content = reader.parse(is);

            return Document.of(content, contentType, targetUri, contextUrl);

        } catch (Exception e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    /**
     * Sets a timeout for HTTP requests. If the response is not received within the
     * specified duration, a {@link JsonLdException} with code
     * {@code LOADING_DOCUMENT_TIMEOUT} is thrown.
     *
     * @param timeout the timeout duration, or {@code null} for no timeout
     * @return this {@link HttpLoader} instance for chaining
     * @since 1.6.1
     */
    public HttpLoader timeout(Duration timeout) {
        client.timeout(timeout);
        return this;
    }

    /**
     * Adds or replaces HTTP headers to be included in every request.
     *
     * @param headers a collection of header name/value pairs
     * @return this {@link HttpLoader} instance for chaining
     * @since 2.0.0
     */
    public HttpLoader headers(Collection<Entry<String, String>> headers) {
        client.headers(headers);
        return this;
    }

    /**
     * Defines which HTTP response content types are accepted by this loader.
     * <p>
     * <p>
     * The provided predicate is evaluated against each response's
     * {@code Content-Type} header. If it returns {@code true}, the response is
     * considered acceptable. By default, the loader accepts
     * {@code application/json}, {@code application/ld+json}, and other
     * {@code +json} media types.
     * </p>
     *
     * <p>
     * The predicate must not be {@code null}. Regardless of the accepted content
     * type, the response body must still be parseable by {@link TreeParser}.
     * </p>
     *
     * @param predicate a non-{@code null} predicate that returns {@code true} for
     *                  accepted media types
     * @return this {@link HttpLoader} instance for method chaining
     * @throws NullPointerException if {@code predicate} is {@code null}
     * @since 2.0.0
     */
    public HttpLoader accept(Predicate<MediaType> predicate) {
        this.acceptContent = Objects.requireNonNull(predicate, "predicate must not be null");
        return this;
    }

    /**
     * Builds a default HTTP {@code Accept} header string for JSON-LD requests.
     *
     * @return a string suitable for use as an HTTP {@code Accept} header
     */
    public static final String acceptHeader() {
        return acceptHeader(List.of());
    }

    /**
     * Builds an HTTP {@code Accept} header that includes JSON-LD and JSON types,
     * optionally with profiles.
     *
     * @param profiles a collection of profile URIs to include in the header, may be
     *                 empty
     * @return a string suitable for use as an HTTP {@code Accept} header
     */
    public static final String acceptHeader(final Collection<String> profiles) {

        final var builder = new StringBuilder().append(MediaType.JSON_LD.toString());

        if (profiles != null && !profiles.isEmpty()) {
            builder
                    .append(";profile=\"")
                    .append(String.join(" ", profiles))
                    .append("\"");
        }

        return builder.append(',')
                .append(MediaType.JSON.toString())
                .append(";q=0.9,*/*;q=0.1")
                .toString();
    }
}
