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
package no.hasmac.jsonld.document;

import java.io.InputStream;
import java.io.Reader;
import java.util.stream.Collectors;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.JsonLdErrorCode;
import no.hasmac.jsonld.http.media.MediaType;
import no.hasmac.rdf.Rdf;

/**
 * @deprecated use {@link JsonDocument#of} or {@link RdfDocument#of} directly
 */
@Deprecated(since = "1.0.4")
public final class DocumentParser {

    private DocumentParser() {
    }

    /**
     * Create a new document.
     *
     * @param contentType {@link MediaType} of the raw content, must not be <code>null</code>
     * @param inputStream providing unparsed raw content described by {{@link MediaType}
     * @return {@link Document} representing unparsed content
     *
     * @throws JsonLdError in a case of parsing error
     */
    public static Document parse(final MediaType contentType, final InputStream inputStream)  throws JsonLdError {

        if (inputStream == null) {
            throw new IllegalArgumentException("The provided content InputStream is null.");
        }

        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }

        if (JsonDocument.accepts(contentType)) {
            return JsonDocument.of(contentType, inputStream);
        }

        if (RdfDocument.accepts(contentType)) {
            return RdfDocument.of(contentType, inputStream);
        }

        throw unsupportedMediaType(contentType);
    }

    /**
     * Create a new document.
     *
     * @param contentType {@link MediaType} of the raw content, must not be <code>null</code>
     * @param reader providing unparsed raw content described by {{@link MediaType}
     * @return {@link Document} representing unparsed content
     *
     * @throws JsonLdError in a case of parsing error
     */
    public static Document parse(final MediaType contentType, final Reader reader)  throws JsonLdError {

        if (reader == null) {
            throw new IllegalArgumentException("The provided content reader is null.");
        }

        if (contentType == null) {
            throw new IllegalArgumentException("The provided content type is null.");
        }

        if (JsonDocument.accepts(contentType)) {
            return JsonDocument.of(contentType, reader);
        }

        if (RdfDocument.accepts(contentType)) {
            return RdfDocument.of(contentType, reader);
        }

        throw unsupportedMediaType(contentType);
    }

    private static JsonLdError unsupportedMediaType(MediaType contentType) throws JsonLdError {
        return new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED,
                "Unsupported media type '" + contentType
                + "'. Supported content types are ["
                + MediaType.JSON_LD + ", "
                + MediaType.JSON  + ", +json, "
                + (Rdf.canRead().stream().map(MediaType::toString).collect(Collectors.joining(", ")))
                + "]"
                );
    }
}
