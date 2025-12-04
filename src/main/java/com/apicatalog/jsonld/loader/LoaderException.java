package com.apicatalog.jsonld.loader;

import java.net.URI;

public class LoaderException extends Exception {

    public enum ErrorCode {

        UNSUPPORTED_SCHEME,
        MULTIPLE_CONTEXT_LINK_HEADERS,
        INVALID_URI,
        BLOCKED_URI,
        INTERNAL,
        UNSUPPORTED_CONTENT_TYPE,
        MISSING_LOCATION_HEADER, 
        TOO_MANY_REDIRECTIONS, 
        
        //TODO ?!?
        REMOTE,
        CLIENT
        

    }

    private static final long serialVersionUID = -4582534653970001228L;

    private final URI uri;
    private final ErrorCode code;

    public LoaderException(ErrorCode code, URI uri) {
        super();
        this.code = code;
        this.uri = uri;
    }

    public LoaderException(ErrorCode code, URI uri, String message) {
        super(message);
        this.code = code;
        this.uri = uri;
    }

    public LoaderException(ErrorCode code, URI uri, Throwable cause) {
        super(cause);
        this.code = code;
        this.uri = uri;
    }

    public LoaderException(URI uri, Throwable cause) {
        super(cause);
        this.code = ErrorCode.INTERNAL;
        this.uri = uri;
    }

    public LoaderException(ErrorCode code, URI uri, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
        this.uri = uri;
    }

    public LoaderException(URI uri, String message, Throwable cause) {
        super(message, cause);
        this.code = ErrorCode.INTERNAL;
        this.uri = uri;
    }

    public URI uri() {
        return uri;
    }

    public ErrorCode code() {
        return code;
    }
}
