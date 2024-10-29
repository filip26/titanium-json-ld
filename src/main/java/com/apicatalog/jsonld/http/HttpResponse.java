package com.apicatalog.jsonld.http;

import java.io.Closeable;
import java.io.InputStream;
import java.util.Collection;
import java.util.Optional;

public interface HttpResponse extends Closeable {

    int statusCode();

    InputStream body();

    /**
     * A links attached to the response
     * @return a collection of links, never <code>null</code>
     */
    Collection<String> links();

    Optional<String> contentType();

    Optional<String> location();

}
