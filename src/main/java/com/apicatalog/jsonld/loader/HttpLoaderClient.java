package com.apicatalog.jsonld.loader;

import java.io.Closeable;
import java.io.InputStream;
import java.net.URI;
import java.time.Duration;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdException;

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
public interface HttpLoaderClient {

    /**
     * Sends an HTTP request to the given target URI and returns a {@link Response}
     * representing the HTTP result.
     * <p>
     * Implementations should typically:
     * <ul>
     * <li>Perform an HTTP GET request to the specified {@code targetUri}</li>
     * <li>Include an {@code Accept} header suitable for JSON-LD content types
     * (e.g., {@code application/ld+json} and related profiles)</li>
     * <li>Apply any headers or timeout previously configured through
     * {@link #headers(Collection)} or {@link #timeout(Duration)}</li>
     * </ul>
     * </p>
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
     * {@link com.apicatalog.jsonld.ErrorCode#LOADING_DOCUMENT_TIMEOUT} may be
     * thrown by the loader.
     * </p>
     *
     * @param timeout the maximum duration to wait for a response, or {@code null}
     *                to disable timeouts
     * @return this {@link HttpLoaderClient} instance for method chaining
     * @throws UnsupportedOperationException if the implementation does not support
     *                                       timeout configuration
     * @since 1.4.0
     */
    default HttpLoaderClient timeout(Duration timeout) {
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
     * @return this {@link HttpLoaderClient} instance for method chaining
     * @throws UnsupportedOperationException if the implementation does not support
     *                                       custom headers
     * @since 2.0.0
     */
    default HttpLoaderClient headers(Collection<Entry<String, String>> headers) {
        throw new UnsupportedOperationException();
    }

    /**
     * Represents an HTTP response returned by
     * {@link HttpLoaderClient#send(URI, Collection)}.
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
