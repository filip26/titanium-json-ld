package com.apicatalog.jsonld.loader;

import java.net.URI;

public class LoaderException extends Exception {

    private static final long serialVersionUID = -4582534653970001228L;

    private final URI uri;
    
    public LoaderException(URI uri, String message) {
        super(message);
        this.uri = uri;
    }

    public LoaderException(URI uri, Throwable cause) {
        super(cause);
        this.uri = uri;
    }

    public LoaderException(URI uri, String message, Throwable cause) {
        super(message, cause);
        this.uri = uri;
    }

    
    public URI uri() {
        return uri;
    }
}
