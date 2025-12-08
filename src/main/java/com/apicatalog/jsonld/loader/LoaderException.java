/*
 * Copyright 2025 the original author or authors.
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

import java.net.URI;

public class LoaderException extends Exception {

    public enum ErrorCode {

        UNSUPPORTED_SCHEME,
        UNSUPPORTED_CONTENT_TYPE,

        INVALID_URI,
        BLOCKED_URI,

        NOT_FOUND,
        FORBIDDEN,        

        // HTTP
        MULTIPLE_CONTEXT_LINK_HEADERS,
        MISSING_LOCATION_HEADER,
        TOO_MANY_REDIRECTIONS,
        REMOTE,
        TIMEOUT,


        UNSPECIFIED,
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
        this.code = ErrorCode.UNSPECIFIED;
        this.uri = uri;
    }

    public LoaderException(ErrorCode code, URI uri, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
        this.uri = uri;
    }

    public LoaderException(URI uri, String message, Throwable cause) {
        super(message, cause);
        this.code = ErrorCode.UNSPECIFIED;
        this.uri = uri;
    }

    public URI uri() {
        return uri;
    }

    public ErrorCode code() {
        return code;
    }
}
