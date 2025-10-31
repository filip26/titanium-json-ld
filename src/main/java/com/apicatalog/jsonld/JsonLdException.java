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
package com.apicatalog.jsonld;

import java.util.Objects;

/**
 * Represents a JSON-LD processing error.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#jsonlderror"> JSON-LD
 *      Error Specification</a>
 */
public final class JsonLdException extends Exception {

    private static final long serialVersionUID = -5538619398631322707L;

    private final JsonLdErrorCode code;

    /**
     * Creates a new {@code JsonLdException} with the specified error code.
     *
     * @param code the error code (must not be {@code null})
     */
    public JsonLdException(JsonLdErrorCode code) {
        super(Objects.requireNonNull(code, "code must not be null").description());
        this.code = code;
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code and
     * message.
     *
     * @param code    the error code (must not be {@code null})
     * @param message the detail message
     */
    public JsonLdException(JsonLdErrorCode code, String message) {
        super(message);
        this.code = Objects.requireNonNull(code, "code must not be null");
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code and
     * cause.
     *
     * @param code  the error code (must not be {@code null})
     * @param cause the cause of the exception
     */
    public JsonLdException(JsonLdErrorCode code, Throwable cause) {
        super(Objects.requireNonNull(code, "code must not be null").description(), cause);
        this.code = code;
    }

    /**
     * Creates a new {@code JsonLdException} with the specified error code, message,
     * and cause.
     *
     * @param code    the error code (must not be {@code null})
     * @param message the detail message
     * @param cause   the cause of the exception
     */
    public JsonLdException(JsonLdErrorCode code, String message, Throwable cause) {
        super(message, cause);
        this.code = Objects.requireNonNull(code, "code must not be null");
    }

    /**
     * Returns the associated {@link JsonLdErrorCode}.
     *
     * @return the JSON-LD error code
     */
    public JsonLdErrorCode code() {
        return code;
    }

    /**
     * Returns a concise string representation optimized for logging.
     */
    @Override
    public String toString() {
        // Avoid String.format()/.formatted() overhead
        return "JsonLdException[" + code.name() + ", message=" + getMessage() + "]";
    }
}
