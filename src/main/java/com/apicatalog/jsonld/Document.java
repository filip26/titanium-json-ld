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

import java.net.URI;
import java.util.Objects;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.web.media.MediaType;

/**
 * A JSON-LD document that can be processed by the processor.
 * 
 * Holds tree alike data strcuture as a content. Library, serialization format
 * agnostic {@link PolyNode}.
 *
 */
public class Document {

    protected final MediaType contentType;
    protected final String profile;
    protected final PolyNode content;

    protected final URI documentUrl;
    protected final URI contextUrl;

    protected Document(
            final PolyNode content,
            final MediaType contentType,
            final String profile,
            final URI documentUrl,
            final URI contextUrl) {
        this.content = Objects.requireNonNull(content);
        this.contentType = contentType;
        this.profile = profile;
        this.documentUrl = documentUrl;
        this.contextUrl = contextUrl;
    }

    /**
     * Create a new document from {@link JsonStructure}. Sets {@link MediaType#JSON}
     * as the content type.
     *
     * @param structure representing parsed JSON content
     * @return {@link Document} representing JSON content
     */
    public static Document of(final PolyNode content) {
        return of(content, null, null, null, null);
    }

    public static Document of(final PolyNode content, final URI documentUrl) {
        return of(content, null, null, documentUrl, null);
    }

    public static Document of(final PolyNode content, final URI documentUrl, URI contextUrl) {
        return of(content, null, null, documentUrl, contextUrl);
    }

    /**
     * Create a new document from {@link JsonStructure}.
     *
     * @param contentType reflecting the provided {@link JsonStructure}, e.g.
     *                    {@link MediaType#JSON_LD}, any JSON based media type is
     *                    allowed
     * @param structure   representing parsed JSON content
     * @return {@link Document} representing JSON content
     */
    public static Document of(final PolyNode content, MediaType contentType) {
        return of(content, contentType, null, null, null);
    }

    public static Document of(final PolyNode content, String profile) {
        return of(content, null, profile, null, null);
    }

    public static Document of(final PolyNode content, MediaType contentType, String profile) {
        return of(content, contentType, profile, null, null);
    }

    public static Document of(final PolyNode content, MediaType contentType, URI documentUrl) {
        return of(content, contentType, null, documentUrl, null);
    }

    public static Document of(final PolyNode content, MediaType contentType, URI documentUrl, URI contextUrl) {
        return of(content, contentType, null, documentUrl, contextUrl);
    }

    public static Document of(final PolyNode content, MediaType contentType, String profile, URI documentUrl) {
        return of(content, contentType, profile, documentUrl, null);
    }

    public static Document of(final PolyNode content, MediaType contentType, String profile, URI documentUrl, URI contextUrl) {
        return new Document(
                Objects.requireNonNull(content),
                contentType != null
                        ? new MediaType(contentType.type(), contentType.subtype())
                        : null,
                profile == null && contentType != null
                        ? contentType.findFirstParameter("profile").orElse(null)
                        : profile,
                documentUrl,
                contextUrl);
    }

    public static final Document fetch(URI uri, DocumentLoader loader, boolean extractAllScripts) throws JsonLdException {

        if (loader == null) {
            throw new JsonLdException(
                    ErrorCode.LOADING_DOCUMENT_FAILED,
                    "Document loader is null. Cannot fetch [" + uri + "].");
        }

        return Optional.ofNullable(
                loader.loadDocument(
                        uri,
                        new DocumentLoader.Options(
                                extractAllScripts,
                                null,
                                null)))
                .orElseThrow(() -> new JsonLdException(
                        ErrorCode.LOADING_DOCUMENT_FAILED,
                        "Returned document is null [" + uri + "]."));
    }

    /**
     * The <a href="https://tools.ietf.org/html/rfc2045#section-5">Content-Type</a>
     * of the loaded document, exclusive of any optional parameters.
     *
     * @return <code>Content-Type</code> of the loaded document, never
     *         <code>null</code>
     */
    public MediaType contentType() {
        return contentType;
    }

    /**
     * The value of the HTTP Link header when profile attribute matches
     * <code>http://www.w3.org/ns/json-ld#context</code>.
     *
     * @return attached {@link URI} referencing document context or
     *         <code>null</code> if not available
     */
    public URI context() {
        return contextUrl;
    }

    /**
     * The final {@link URI} of the loaded document.
     *
     * @return {@link URI} of the loaded document or <code>null</code> if not
     *         available
     */
    public URI url() {
        return documentUrl;
    }

    /**
     * The value of any <code>profile</code> parameter retrieved as part of the
     * original {@link #contentType()}.
     *
     * @return document profile or {@link Optional#empty()}
     */
    public String profile() {
        return profile;
    }

    /**
     * Get the document parsed content.
     *
     * @return {@link PolyNode} representing materialized document content
     */
    public PolyNode content() {
        return content;
    }
}
