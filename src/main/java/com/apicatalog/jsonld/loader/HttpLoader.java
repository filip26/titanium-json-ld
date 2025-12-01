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

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
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
 * The loader uses a configurable {@link Client} to send HTTP requests and a
 * {@link TreeParser} to parse the retrieved content into {@link Document}
 * instances.
 * </p>
 *
 * @see DocumentLoader
 * @see Client
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
     * </p>
     * <ul>
     * <li>{@code application/ld+json}</li>
     * <li>{@code application/json}</li>
     * <li>or any media type whose subtype ends with {@code +json}</li>
     * </ul>
     * A {@code null} input always results in {@code false}.
     *
     * <p>
     * Used by default when no custom predicate is supplied through
     * {@link HttpLoader#accept(java.util.function.Predicate)}.
     * </p>
     *
     * @since 2.0.0
     */
    public static final Predicate<MediaType> JSON_LD_CONTENT = contentType -> contentType != null
            && (MediaType.JSON_LD.match(contentType)
                    || MediaType.JSON.match(contentType)
                    || contentType.subtype().toLowerCase().endsWith(PLUS_JSON));

    /**
     * Default vendor headers added to every HTTP request.
     * 
     * {@link HttpLoader#headers(Collection)}
     */
    public static final Collection<Entry<String, String>> VENDOR_HEADERS = List.of(
            Map.entry(
                    "User-Agent",
                    "titanium-json-ld/2 (+https://github.com/filip26/titanium-json-ld)"));

    /**
     * Default maximum number of allowed HTTP redirections.
     */
    public static final int MAX_REDIRECTIONS = 10;

    private final Client client;

    private final Map<MediaType, TreeParser> parsers;
    private final TreeParser defaultParser;

    private Predicate<MediaType> acceptContent;
    private int maxRedirections;

    /**
     * Constructs a new {@link HttpLoader}.
     *
     * @param client  the HTTP client used for network requests
     * @param parser  the default parser for decoding JSON documents
     * @param parsers the content-type to parser mapping table
     */
    protected HttpLoader(Client client, TreeParser parser, Map<MediaType, TreeParser> parsers) {
        this.client = client;
        this.defaultParser = parser;
        this.parsers = parsers;
        this.maxRedirections = MAX_REDIRECTIONS;
        this.acceptContent = JSON_LD_CONTENT;
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
                parser,
                Map.of());
    }

    /**
     * Creates a new {@link HttpLoader} with a default {@link HttpClient}.
     *
     * @param parser  the {@link TreeParser} used to parse retrieved content
     * @param parsers the content-type to parser mapping table
     * @return a new configured {@link HttpLoader}
     */
    public static HttpLoader newLoader(final TreeParser parser, Map<MediaType, TreeParser> parsers) {
        return newLoader(
                HttpClient
                        .newBuilder()
                        .followRedirects(Redirect.NEVER)
                        .build(),
                parser,
                parsers);
    }

    /**
     * Creates a new {@link HttpLoader} using the given {@link HttpClient} and
     * parser.
     *
     * @param client the HTTP client to use for network calls
     * @param parser the JSON parser used to parse responses
     * @return a configured {@link HttpLoader} instance
     */
    public static HttpLoader newLoader(final HttpClient client, TreeParser parser) {
        return newLoader(client, parser, Map.of());
    }

    /**
     * Creates a new {@link HttpLoader} using the given {@link HttpClient} and
     * parser.
     *
     * @param client  the HTTP client to use for network calls
     * @param parser  the JSON parser used to parse responses
     * @param parsers the content-type to parser mapping table
     * @return a configured {@link HttpLoader} instance
     */
    public static HttpLoader newLoader(final HttpClient client, TreeParser parser, Map<MediaType, TreeParser> parsers) {
        return newLoader(new JavaHttpClient(client), parser, parsers);
    }

    /**
     * Creates a new {@link HttpLoader} using the given {@link Client} and parser.
     *
     * @param client the HTTP client to use for network calls
     * @param reader the JSON parser used to parse responses
     * @return a configured {@link HttpLoader} instance
     */
    public static HttpLoader newLoader(final Client client, TreeParser reader) {
        return newLoader(client, reader, Map.of());
    }

    /**
     * Creates a new {@link HttpLoader} using the given {@link Client} and parser.
     *
     * @param client  the HTTP client to use for network calls
     * @param parser  the JSON parser used to parse responses
     * @param parsers the content-type to parser mapping table
     * @return a configured {@link HttpLoader} instance
     */
    public static HttpLoader newLoader(final Client client, TreeParser parser, Map<MediaType, TreeParser> parsers) {
        return new HttpLoader(client, parser, parsers).headers(VENDOR_HEADERS);
    }

    /**
     * Loads a remote JSON-LD document from the given URI according to the JSON-LD
     * specification.
     * <p>
     * This method handles:
     * </p>
     * <ul>
     * <li>Redirections (301, 302, 303, 307) up to {@link #maxRedirections}</li>
     * <li>Content type negotiation and validation</li>
     * <li>Processing of HTTP Link headers for alternate or context URLs</li>
     * </ul>
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

                        final var location = response.location();

                        if (location.isPresent()) {
                            targetUri = UriResolver.resolveAsUri(targetUri, location.get());
                            continue;
                        }

                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED,
                                """
                                        HTTP Location header is required for status code=%d but is not present, url=%s
                                        """.formatted(response.statusCode(), url));
                    }

                    if (response.statusCode() != 200) {
                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED,
                                """
                                        Unexpected HTTP response status code=%d, url=%s
                                        """.formatted(response.statusCode(), url));
                    }

                    contentType = response.contentType()
                            .map(MediaType::of)
                            .orElse(null);

                    final var linkValues = response.links();

                    if (linkValues != null && !linkValues.isEmpty()) {

                        // 4.
                        if (contentType == null
                                || (!MediaType.JSON.match(contentType)
                                        && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))) {

                            final var baseUri = targetUri;

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

                            final var baseUri = targetUri;

                            final List<Link> contextUris = linkValues.stream()
                                    .flatMap(l -> Link.of(l, baseUri).stream())
                                    .filter(l -> l.relations().contains(Terms.PROFILE_CONTEXT))
                                    .collect(Collectors.toList());

                            if (contextUris.size() > 1) {
                                throw new JsonLdException(ErrorCode.MULTIPLE_CONTEXT_LINK_HEADERS,
                                        """
                                                Only one context link header is allowed but got %s, url=%s
                                                """.formatted(contextUris, url));

                            } else if (contextUris.size() == 1) {
                                contextUri = contextUris.get(0).target();
                            }
                        }
                    }

                    if (contentType == null) {
                        LOGGER.log(Level.WARNING,
                                "HTTP GET on URL [{0}] does not return content-type header.",
                                url);

                    } else if (!acceptContent.test(contentType)) {
                        throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED,
                                """
                                        Unsupported content-type=%s, url=%s
                                        """.formatted(contentType, url));
                    }

                    return read(contentType, targetUri, contextUri, response);
                }
            }

            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED,
                    """
                            Too many redirections detected, maximum=%d, url=%s
                            """.formatted(maxRedirections, url));

        } catch (IOException e) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED, e);
        }
    }

    private final Document read(
            final MediaType contentType,
            final URI targetUri,
            final URI contextUrl,
            final Response response) throws JsonLdException {

        final var parser = contentType == null
                ? defaultParser
                : parsers.getOrDefault(contentType, defaultParser);

        if (parser == null) {
            throw new JsonLdException(ErrorCode.LOADING_DOCUMENT_FAILED,
                    """
                            Response content-type=%s cannot be parsed, parser is not defined, url=%s
                            """.formatted(contentType, targetUri));
        }

        try (final var is = response.body()) {

            final var content = parser.parse(is);

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
     * 
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

    public HttpLoader maxRedirections(int maxRedirections) {
        this.maxRedirections = maxRedirections;
        return this;
    }

    public int maxRedirections() {
        return maxRedirections;
    }

    /**
     * Low-level HTTP transport component used internally by {@link HttpLoader},
     * which is an implementation of the
     * {@link com.apicatalog.jsonld.loader.DocumentLoader} interface.
     * <p>
     * {@code HttpLoaderClient} defines how HTTP requests are executed and how
     * responses are represented when loading remote JSON-LD documents. It is not
     * used directly by JSON-LD processors, but by higher-level loaders such as
     * {@link HttpLoader}.
     * </p>
     *
     * <p>
     * Implementations are responsible for sending HTTP requests, handling
     * redirects, and exposing response data (status code, headers, and body stream)
     * via the {@link Response} interface.
     * </p>
     *
     * @see HttpLoader
     * @see com.apicatalog.jsonld.loader.DocumentLoader
     */
    public interface Client {

        /**
         * Sends an HTTP request to the given target URI and returns a {@link Response}
         * representing the HTTP result.
         * <p>
         * Implementations should typically:
         * </p>
         * <ul>
         * <li>Perform an HTTP GET request to the specified {@code targetUri}</li>
         * <li>Include an {@code Accept} header suitable for JSON-LD content types
         * (e.g., {@code application/ld+json} and related profiles)</li>
         * <li>Apply any headers or timeout previously configured through
         * {@link #headers(Collection)} or {@link #timeout(Duration)}</li>
         * </ul>
         *
         * @param url             the absolute URL of the remote resource to request
         * @param requestProfiles JSON-LD profile URIs to include in the {@code Accept}
         *                        header, may be empty
         * @return a {@link Response} representing the HTTP response, including headers
         *         and body stream
         * @throws JsonLdException if the request fails or cannot be processed
         */
        Response send(URI url, Collection<String> requestProfiles) throws JsonLdException;

        /**
         * Configures the read timeout for HTTP requests made by this client.
         * <p>
         * If a server does not respond within the specified duration, a
         * {@link JsonLdException} with code
         * {@link com.apicatalog.jsonld.JsonLdException.ErrorCode#LOADING_DOCUMENT_TIMEOUT}
         * may be thrown by the loader.
         * </p>
         *
         * @param timeout the maximum duration to wait for a response, or {@code null}
         *                to disable timeouts
         * @return this {@link Client} instance for method chaining
         * @throws UnsupportedOperationException if the implementation does not support
         *                                       timeout configuration
         * @since 1.4.0
         */
        default Client timeout(Duration timeout) {
            throw new UnsupportedOperationException();
        }

        /**
         * Sets additional HTTP headers to be included in all requests made by this
         * client.
         * <p>
         * These headers supplement or override default headers such as
         * {@code User-Agent} or {@code Accept}.
         * </p>
         *
         * @param headers a collection of HTTP header entries (name/value pairs)
         * @return this {@link Client} instance for method chaining
         * @throws UnsupportedOperationException if the implementation does not support
         *                                       custom headers
         * @since 2.0.0
         */
        default Client headers(Collection<Entry<String, String>> headers) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Represents an HTTP response returned by {@link Client#send(URI, Collection)}.
     * <p>
     * The {@link HttpLoader} uses this data to interpret content types, detect
     * redirects, parse link headers, and read the response body as a JSON-LD
     * document or remote context.
     * </p>
     * <p>
     * Implementations must ensure that associated resources (such as network
     * streams) are released when {@link #close()} is called.
     * </p>
     */
    public interface Response extends Closeable {

        /**
         * Returns the response body as an {@link InputStream}.
         * <p>
         * The caller is responsible for reading and closing this stream. Closing the
         * {@link Response} must also close this stream.
         * </p>
         *
         * @return an input stream providing the response body
         */
        InputStream body();

        /**
         * Returns the raw values of all {@code Link} headers present in the response.
         * <p>
         * The {@link HttpLoader} parses these values to discover alternate
         * representations or external context documents.
         * </p>
         *
         * @return a collection of raw {@code Link} header values, or an empty
         *         collection if none are present
         */
        Collection<String> links();

        /**
         * Returns the MIME type of the response as indicated by the
         * {@code Content-Type} header, if present.
         *
         * @return an {@link Optional} containing the content type value, or empty if
         *         not specified
         */
        Optional<String> contentType();

        /**
         * Returns the {@code Location} header value, typically used to handle HTTP
         * redirections (3xx status codes).
         *
         * @return an {@link Optional} containing the location URI as a string, or empty
         *         if not provided
         */
        Optional<String> location();

        /**
         * Returns the HTTP status code of this response.
         *
         * @return the numeric HTTP status code (e.g. 200, 301, 404)
         */
        int statusCode();
    }
}
