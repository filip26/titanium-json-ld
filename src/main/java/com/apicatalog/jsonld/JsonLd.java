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

import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.jsonld.api.CompactionApi;
import com.apicatalog.jsonld.api.FlatteningApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.TreeDocument;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.fromrdf.QuadsToJsonLd;
import com.apicatalog.jsonld.processor.Compactor;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.processor.Framer;
import com.apicatalog.jsonld.processor.RdfEmitter;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.web.uri.UriUtils;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;

/**
 * The {@link JsonLd} interface is the high-level programming structure that
 * developers use to access the JSON-LD transformation methods. This class
 * provides methods to process JSON-LD.
 *
 * All the methods in this class are thread-safe.
 */
public final class JsonLd {

    private static final String DOCUMENT_LOCATION_PARAM_NAME = "documentLocation";
    private static final String DOCUMENT_URI_PARAM_NAME = "documentUri";
    private static final String DOCUMENT_PARAM_NAME = "document";
    private static final String CONTEXT_PARAM_NAME = "context";
    private static final String CONTEXT_LOCATION_PARAM_NAME = "contextLocation";
    private static final String CONTEXT_URI_PARAM_NAME = "contextUri";
    private static final String FRAME_LOCATION_PARAM_NAME = "frameLocation";
    private static final String FRAME_URI_PARAM_NAME = "frameUri";
    private static final String FRAME_PARAM_NAME = "frame";

    private JsonLd() {
    }

    /**
     * Expands the referenced document. DocumentLoader must be set in order to fetch
     * the referenced document. See JsonLdOptions.loader method.
     *
     * @param document {@link URI} referencing JSON-LD document to expand
     * @param options
     * @return
     * @throws JsonLdException
     */
    public static final Collection<?> expand(final URI document, final JsonLdOptions options) throws JsonLdException, IOException {
        return expand(
                TreeDocument.fetch(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                options);
    }

    /**
     * Expands the provided remote document.
     *
     * @param document to expand
     * @param options
     * @return {@link ExpansionApi} allowing to set additional parameters
     * @throws JsonLdException
     */
    public static final Collection<?> expand(final Document document, final JsonLdOptions options) throws JsonLdException, IOException {
        return Expander.expand(document, options);
//        return new ExpansionApi(assertJsonDocument(document, DOCUMENT_PARAM_NAME));
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param document {@link URI} referencing JSON-LD document to compact
     * @param context  {@link URI} referencing the context to use when compacting
     *                 the document
     * @param options
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final Map<String, ?> compact(final URI document, final URI context, final JsonLdOptions options) throws JsonLdException, IOException {
        return Compactor.compact(
                TreeDocument.fetch(document, options.loader(), options.isExtractAllScripts()),
                TreeDocument.fetch(context, options.loader(), options.isExtractAllScripts()),
                options);
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param document {@code URI} referencing JSON-LD document to compact
     * @param context  {@link Document} representing the context or
     *                 {@link JsonArray} consisting of one or many
     *                 {@link JsonObject} and {@link JsonString} referencing the
     *                 context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final URI document, final Document context) {
        return new CompactionApi(
                assertUri(document, DOCUMENT_URI_PARAM_NAME),
                assertJsonDocument(context, CONTEXT_PARAM_NAME));
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param context  JSON-LD document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final Document document, final Document context) {
        return new CompactionApi(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertJsonDocument(context, CONTEXT_PARAM_NAME));
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document   to compact
     * @param contextUri {@link URI} referencing the context to use when compacting
     *                   the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final Document document, final URI contextUri) {
        return new CompactionApi(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertUri(contextUri, CONTEXT_URI_PARAM_NAME));
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static final FlatteningApi flatten(final URI documentUri) {
        return new FlatteningApi(assertUri(documentUri, DOCUMENT_URI_PARAM_NAME));
    }

    /**
     * Flattens the remote input and optionally compacts it using context.
     *
     * @param document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static final FlatteningApi flatten(final Document document) {
        return new FlatteningApi(assertJsonDocument(document, DOCUMENT_PARAM_NAME));
    }

    /**
     * Frames the given remote input using remote frame.
     *
     * @param document {@code URI} referencing JSON-LD document to frame
     * @param frame    {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final URI document, final URI frame, final JsonLdOptions options) throws JsonLdException, IOException {
        return Framer.frame(
                TreeDocument.fetch(document, options.loader(), options.isExtractAllScripts()),
                Frame.of(TreeDocument.fetch(frame, options.loader(), options.isExtractAllScripts()), options),
                options);
    }

    /**
     * Frames the local document using given local frame.
     *
     * @param document to frame
     * @param frame    JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     * @throws IOException
     * @throws JsonLdException
     */
    public static final Map<String, ?> frame(final Document document, final Document frame, final JsonLdOptions options) throws JsonLdException, IOException {
        return Framer.frame(document, Frame.of(document, options), options);
//        return new FramingApi(
//                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
//                assertJsonDocument(frame, FRAME_PARAM_NAME));
    }

    /**
     * Frames the remote input using given remote frame.
     *
     * @param document {@code IRI} referencing JSON-LD document to frame
     * @param frame    {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final Document document, final URI frame, final JsonLdOptions options) throws JsonLdException, IOException {
        return Framer.frame(
                document,
                Frame.of(TreeDocument.fetch(frame, options.loader(), options.isExtractAllScripts()), options),
                options);
    }

    /**
     * Frames the remote input using given local frame.
     *
     * @param document {@code URI} referencing JSON-LD document to frame
     * @param frame    JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final URI document, final Document frame, final JsonLdOptions options) throws JsonLdException, IOException {
        return Framer.frame(
                TreeDocument.fetch(document, options.loader(), options.isExtractAllScripts()),
                Frame.of(frame, options),
                options);
    }

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final void toRdf(final URI document, RdfQuadConsumer consumer, JsonLdOptions options) throws JsonLdException, IOException {
        toRdf(TreeDocument
                .fetch(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                consumer,
                options);
    }

    /**
     * Transforms {@link Document} into {@link RdfDataset}.
     * 
     * @param document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final void toRdf(final Document document, RdfQuadConsumer consumer, JsonLdOptions options) throws JsonLdException, IOException {
        RdfEmitter.toRdf(document, consumer, options);
    }

    /**
     * Transforms an RDF quad set into a JSON-LD document in expanded form.
     * <p>
     * Use {@link NQuadsReader} to read quads, or manually add them to
     * {@link QuadsToJsonLd} by calling
     * {@link QuadsToJsonLd#quad(String, String, String, String, String, String, String)}.
     * Retrieve the expanded JSON-LD document by calling
     * {@link QuadsToJsonLd#toJsonLd()}.
     * </p>
     * <p>
     * <strong>Note:</strong> {@link QuadsToJsonLd} adopts the
     * {@link RdfQuadConsumer} interface, allowing integration with Jena, Jelly,
     * RDF4J, and other RDF libraries.
     * </p>
     * 
     * @since 1.7.0
     * 
     * @return a {@link QuadsToJsonLd} instance, allowing additional parameter
     *         configuration
     */
    public static final QuadsToJsonLd fromRdf() {
        return new QuadsToJsonLd();
    }

    public static final QuadsToJsonLd fromRdf(JsonLdOptions options) {
        return new QuadsToJsonLd().options(options);
    }

    private static final URI assertLocation(final String location, final String param) {

        assertNotNull(location, param);

        if (location == null || location.isBlank()) {
            throw new IllegalArgumentException("'" + param + "' is blank string.");
        }

        final URI uri = UriUtils.create(location.strip());

        if (uri == null || !uri.isAbsolute()) {
            throw new IllegalArgumentException("'" + param + "' is not an absolute URI [" + location + "].");
        }

        return uri;
    }

    private static final URI assertUri(final URI uri, final String param) {

        assertNotNull(uri, param);

        if (!uri.isAbsolute()) {
            throw new IllegalArgumentException("'" + param + "' is not an absolute URI [" + uri + "].");
        }

        return uri;
    }

    private static final Document assertJsonDocument(final Document document, final String param) {

        assertNotNull(document, param);

        if (!document.getJsonContent().isPresent()) {
            throw new IllegalArgumentException("'" + param + "' is not not JSON document but [" + document.contentType() + "].");
        }
        return document;
    }

    private static final void assertNotNull(Object value, final String param) {
        if (value == null) {
            throw new IllegalArgumentException("'" + param + "' is null.");
        }
    }
}
