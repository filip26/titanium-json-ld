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
package no.hasmac.jsonld;

/**
 * The {@link JsonLdError} type is used to report processing errors.
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#jsonlderror">JsonLdError
 *      Specification</a>
 *
 */
public final class JsonLdError extends Exception {

    private static final long serialVersionUID = -1912600269069309493L;

    private final JsonLdErrorCode code;

    public JsonLdError(JsonLdErrorCode code) {
        super(code.toMessage());
        this.code = code;
    }

    public JsonLdError(JsonLdErrorCode code, String message) {
        super(message);
        this.code = code;
    }

    public JsonLdError(JsonLdErrorCode code, Throwable cause) {
        super(code.toMessage(), cause);
        this.code = code;
    }

    public JsonLdErrorCode getCode() {
        return code;
    }

    @Override
    public String toString() {
        return "JsonLdError[code=" + code.toMessage() + ", message=" + getMessage() + "]";
    }
}
