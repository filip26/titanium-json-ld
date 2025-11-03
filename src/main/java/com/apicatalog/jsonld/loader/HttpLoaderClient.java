package com.apicatalog.jsonld.loader;

import java.io.Closeable;
import java.io.InputStream;
import java.net.URI;
import java.time.Duration;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdException;

public interface HttpLoaderClient {

    Response send(URI targetUri, Collection<String> requestProfiles) throws JsonLdException;

    /**
     * Configure read timeout
     * 
     * @param timeout to set or <code>null</code> for no timeout
     * 
     * @return {@link HttpLoaderClient} instance,
     * 
     * @since 1.4.0
     */
    default HttpLoaderClient timeout(Duration timeout) {
        throw new UnsupportedOperationException();
    }

    /**
     * Sets additional headers added to each request.
     * 
     * @param headers
     * @return {@link HttpLoaderClient} instance,
     * 
     * @since 2.0.0
     * 
     */
    default HttpLoaderClient headers(Collection<Entry<String, String>> headers) {
        throw new UnsupportedOperationException();
    }

    public interface Response extends Closeable {

        InputStream body();

        Collection<String> links();

        Optional<String> contentType();

        Optional<String> location();

        int statusCode();
    }

}
