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
package com.apicatalog.jsonld.document;

import java.io.Reader;
import java.net.URI;
import java.util.Objects;
import java.util.Optional;

import com.apicatalog.jsonld.JsonLdErrorCode;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.tree.io.PolyNode;
import com.apicatalog.web.media.MediaType;

public final class RemoteDocument implements Document {

    private static final String PLUS_JSON = "+json";

    private final MediaType contentType;
    private final String profile;
    private final PolyNode content;

    private URI documentUrl;
    private URI contextUrl;

    private RemoteDocument(final MediaType type, final String profile, final PolyNode content) {
        this.contentType = type;
        this.profile = profile;
        this.content = content;
    }

    public static RemoteDocument of(final PolyNode content, final URI documentUrl) {
        var document = new RemoteDocument(null, null, Objects.requireNonNull(content));
        document.documentUrl = documentUrl;
        return document;
    }

    /* ----------------- */

    /**
     * Create a new document from {@link JsonStructure}. Sets {@link MediaType#JSON}
     * as the content type.
     *
     * @param structure representing parsed JSON content
     * @return {@link Document} representing JSON content
     */
    public static RemoteDocument of(final PolyNode content) {
        Objects.requireNonNull(content);
        return of(MediaType.JSON, content);
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
    public static RemoteDocument of(final MediaType contentType, final PolyNode content) {

        Objects.requireNonNull(content);

        // FIXME
        return new RemoteDocument(
                new MediaType(contentType.type(), contentType.subtype()),
                contentType.findFirstParameter("profile").orElse(null), content);

//        if (contentType == null) {
//            throw new IllegalArgumentException("The provided JSON type is null.");
//        }
//
//        assertContentType(contentType);
//
//        if (content == null) {
//            throw new IllegalArgumentException("The provided JSON structure is null.");
//        }

//        return new TreeDocument(
//                new MediaType(contentType.type(), contentType.subtype()), 
//                contentType.findFirstParameter("profile").orElse(null), structure);
    }

    /**
     * Create a new document from content provided by {@link InputStream}. Sets
     * {@link MediaType#JSON} as the content type.
     *
     * @param is representing parsed JSON content
     * @return {@link Document} representing JSON document
     */
//    @Deprecated
//    public static final RemoteDocument of(final InputStream is) throws JsonLdException {
//        return of(MediaType.JSON, is);
//    }

    /**
     * Create a new document from content provided by {@link InputStream}.
     *
     * @param contentType reflecting the provided {@link InputStream} content, e.g.
     *                    {@link MediaType#JSON_LD}, any JSON based media type is
     *                    allowed
     * @param is          providing JSON content
     * @return {@link Document} representing JSON document
     *
     * @throws JsonLdException if the document creation fails
     */
//    @Deprecated
//    public static final RemoteDocument of(final MediaType contentType, final InputStream is) throws JsonLdException {
//
//        assertContentType(contentType);
//
//        if (is == null) {
//            throw new IllegalArgumentException("The input stream parameter cannot be null.");
//        }
//
//        try (final JsonParser parser = JsonProvider.instance().createParser(is)) {
//
//            return doParse(contentType, parser);
//
//        } catch (JsonException e) {
//            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
//        }
//    }

    /**
     * Create a new document from content provided by {@link Reader}. Sets
     * {@link MediaType#JSON} as the content type.
     *
     * @param reader providing JSON content
     * @return {@link Document} representing JSON document
     */
//    @Deprecated
//    public static final RemoteDocument of(final Reader reader) throws JsonLdException {
//        return of(MediaType.JSON, reader);
//    }

    /**
     * Create a new document from content provided by {@link Reader}.
     *
     * @param contentType reflecting the provided content, e.g.
     *                    {@link MediaType#JSON_LD}, any JSON based media type is
     *                    allowed
     * @param reader      providing JSON content
     * @return {@link Document} representing JSON document
     *
     * @throws JsonLdException if the document creation fails
     */
//    @Deprecated
//    public static final RemoteDocument of(final MediaType contentType, final Reader reader) throws JsonLdException {
//
//        assertContentType(contentType);
//
//        if (reader == null) {
//            throw new IllegalArgumentException("The reader parameter cannot be null.");
//        }
//
//        try (final JsonParser parser = JsonProvider.instance().createParser(reader)) {
//
//            return doParse(contentType, parser);
//
//        } catch (JsonException e) {
//            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
//        }
//    }

//    @Deprecated
//    private static final RemoteDocument doParse(
//            final MediaType contentType,
//            final JsonParser parser) throws JsonLdException {
//
//        if (!parser.hasNext()) {
//            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Nothing to read. Provided document is empty.");
//        }
//
//        parser.next();
//
//        final JsonValue root = parser.getValue();
//
//        final String profile = contentType.findFirstParameter("profile").orElse(null);
//
//        if (JsonUtils.isArray(root)) {
//            return new RemoteDocument(contentType, profile,
//                    new PolyNode(root.asJsonArray(), JakartaAdapter.instance()));
//        }
//
//        if (JsonUtils.isObject(root)) {
//            return new RemoteDocument(new MediaType(contentType.type(), contentType.subtype()), profile,
//                    new PolyNode(root.asJsonObject(), JakartaAdapter.instance()));
//        }
//
//        throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "JSON document's top level element must be JSON array or object.");
//    }

    public static final boolean accepts(final MediaType contentType) {
        return contentType != null &&
                (MediaType.JSON_LD.match(contentType)
                        || MediaType.JSON.match(contentType)
                        || contentType.subtype().toLowerCase().endsWith(PLUS_JSON));
    }

    private static final void assertContentType(final MediaType contentType) {
        if (!accepts(contentType)) {
            throw new IllegalArgumentException(
                    "Unsupported media type '" + contentType
                            + "'. Supported content types are ["
                            + MediaType.JSON_LD + ", "
                            + MediaType.JSON + ", +json]");
        }
    }

    public static final Document fetch(URI uri, DocumentLoader loader, boolean extractAllScripts) throws JsonLdException {

        if (loader == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Document loader is null. Cannot fetch [" + uri + "].");
        }

        final var loaderOptions = new DocumentLoader.Options(
                extractAllScripts,
                null,
                null);

        final Document remoteDocument = loader.loadDocument(uri, loaderOptions);

        if (remoteDocument == null) {
            throw new JsonLdException(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Returned document is null [" + uri + "].");
        }
        return remoteDocument;
    }

    @Override
    public MediaType contentType() {
        return contentType;
    }

    @Override
    public URI contextUrl() {
        return contextUrl;
    }

    @Override
    public void setContextUrl(URI contextUrl) {
        this.contextUrl = contextUrl;
    }

    @Override
    public URI documentUrl() {
        return documentUrl;
    }

    @Override
    public void setDocumentUrl(URI documentUrl) {
        this.documentUrl = documentUrl;
    }

    @Override
    public Optional<String> profile() {
        return Optional.ofNullable(profile);
    }

    @Override
    public PolyNode content() {
        return content;
    }
}
