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

import com.apicatalog.jsonld.api.CompactionApi;
import com.apicatalog.jsonld.api.ExpansionApi;
import com.apicatalog.jsonld.api.FlatteningApi;
import com.apicatalog.jsonld.api.FramingApi;
import com.apicatalog.jsonld.api.StringUtils;
import com.apicatalog.jsonld.api.ToRdfApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.serialization.QuadsToJsonld;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.api.RdfQuadConsumer;

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
     * Expands the referenced document.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters
     */
    public static final ExpansionApi expand(final String documentLocation) {
        return new ExpansionApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Expands the referenced document.
     *
     * @param documentUri {@link URI} referencing JSON-LD document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters
     */
    public static final ExpansionApi expand(final URI documentUri) {
        return new ExpansionApi(assertUri(documentUri, DOCUMENT_URI_PARAM_NAME));
    }

    /**
     * Expands the provided remote document.
     *
     * @param document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters
     */
    public static final ExpansionApi expand(final Document document) {
        return new ExpansionApi(assertJsonDocument(document, DOCUMENT_PARAM_NAME));
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param contextLocation  {@code IRI} referencing the context to use when
     *                         compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final String documentLocation, final String contextLocation) {
        return compact(
                assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME),
                assertLocation(contextLocation, CONTEXT_LOCATION_PARAM_NAME));
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentUri {@link URI} referencing JSON-LD document to compact
     * @param contextUri  {@link URI} referencing the context to use when compacting
     *                    the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final URI documentUri, final URI contextUri) {
        return new CompactionApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
                assertUri(contextUri, CONTEXT_URI_PARAM_NAME));
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param context          {@link Document} representing the context or
     *                         {@link JsonArray} consisting of {@link JsonObject}
     *                         and {@link JsonString} referencing the context to use
     *                         when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final String documentLocation, final Document context) {
        return new CompactionApi(
                assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME),
                assertJsonDocument(context, CONTEXT_PARAM_NAME));
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to compact
     * @param context     {@link Document} representing the context or
     *                    {@link JsonArray} consisting of one or many
     *                    {@link JsonObject} and {@link JsonString} referencing the
     *                    context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final URI documentUri, final Document context) {
        return new CompactionApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
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
     * @param document        to compact
     * @param contextLocation {@code IRI} referencing the context to use when
     *                        compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final Document document, final String contextLocation) {
        return compact(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertLocation(contextLocation, CONTEXT_LOCATION_PARAM_NAME));
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
     * Compacts {@link Document} document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param contextUri       {@link URI} referencing the context to use when
     *                         compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final String documentLocation, final URI contextUri) {
        return new CompactionApi(
                assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME),
                assertUri(contextUri, CONTEXT_URI_PARAM_NAME));
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param documentUri     {@code URI} referencing JSON-LD document to compact
     * @param contextLocation {@code IRI} referencing the context to use when
     *                        compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static final CompactionApi compact(final URI documentUri, final String contextLocation) {
        return new CompactionApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
                assertLocation(contextLocation, CONTEXT_LOCATION_PARAM_NAME));
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static final FlatteningApi flatten(final String documentLocation) {
        return new FlatteningApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
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
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frameUri    {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final URI documentUri, final URI frameUri) {
        return new FramingApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
                assertUri(frameUri, FRAME_URI_PARAM_NAME));
    }

    /**
     * Frames the given remote input using remote frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frameLocation    {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final String documentLocation, final String frameLocation) {
        return new FramingApi(
                assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME),
                assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME));
    }

    /**
     * Frames the local document using given local frame.
     *
     * @param document to frame
     * @param frame    JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final Document document, final Document frame) {
        return new FramingApi(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertJsonDocument(frame, FRAME_PARAM_NAME));
    }

    /**
     * Frames the local document using given remote frame.
     *
     * @param document      to frame
     * @param frameLocation {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final Document document, final String frameLocation) {
        return new FramingApi(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME));
    }

    /**
     * Frames the local document using given remote frame.
     *
     * @param document to frame
     * @param frameUri {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final Document document, final URI frameUri) {
        return new FramingApi(
                assertJsonDocument(document, DOCUMENT_PARAM_NAME),
                assertUri(frameUri, FRAME_URI_PARAM_NAME));
    }

    /**
     * Frames the remote input using given local frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frame            JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final String documentLocation, final Document frame) {
        return new FramingApi(
                assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME),
                assertJsonDocument(frame, FRAME_PARAM_NAME));
    }

    /**
     * Frames the remote input using given remote frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frameUri         {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final String documentLocation, final URI frameUri) {
        return new FramingApi(
                assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME),
                assertUri(frameUri, FRAME_URI_PARAM_NAME));
    }

    /**
     * Frames the remote input using given local frame.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frame       JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final URI documentUri, final Document frame) {
        return new FramingApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
                assertJsonDocument(frame, FRAME_PARAM_NAME));
    }

    /**
     * Frames the remote input using given remote frame.
     *
     * @param documentUri   {@code URI} referencing JSON-LD document to frame
     * @param frameLocation {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final FramingApi frame(final URI documentUri, final String frameLocation) {
        return new FramingApi(
                assertUri(documentUri, DOCUMENT_URI_PARAM_NAME),
                assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME));
    }

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final ToRdfApi toRdf(final String documentLocation) {
        return new ToRdfApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final ToRdfApi toRdf(final URI documentUri) {
        return new ToRdfApi(assertUri(documentUri, DOCUMENT_URI_PARAM_NAME));
    }

    /**
     * Transforms {@link Document} into {@link RdfDataset}.
     * 
     * @param document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final ToRdfApi toRdf(final Document document) {
        return new ToRdfApi(assertJsonDocument(document, DOCUMENT_PARAM_NAME));
    }

    /**
     * Transforms an RDF quad set into a JSON-LD document in expanded form.
     * <p>
     * Use {@link NQuadsReader} to read quads, or manually add them to
     * {@link QuadsToJsonld} by calling
     * {@link QuadsToJsonld#quad(String, String, String, String, String, String, String)}.
     * Retrieve the expanded JSON-LD document by calling
     * {@link QuadsToJsonld#toJsonLd()}.
     * </p>
     * <p>
     * <strong>Note:</strong> {@link QuadsToJsonld} adopts the
     * {@link RdfQuadConsumer} interface, allowing integration with Jena, Jelly,
     * RDF4J, and other RDF libraries.
     * </p>
     * 
     * @since 1.7.0
     * 
     * @return a {@link QuadsToJsonld} instance, allowing additional parameter
     *         configuration
     */
    public static final QuadsToJsonld fromRdf() {
        return new QuadsToJsonld();
    }

    private static final URI assertLocation(final String location, final String param) {

        assertNotNull(location, param);

        if (StringUtils.isBlank(location)) {
            throw new IllegalArgumentException("'" + param + "' is blank string.");
        }

        final URI uri = UriUtils.create(StringUtils.strip(location));

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
            throw new IllegalArgumentException("'" + param + "' is not not JSON document but [" + document.getContentType() + "].");
        }
        return document;
    }

    private static final void assertNotNull(Object value, final String param) {
        if (value == null) {
            throw new IllegalArgumentException("'" + param + "' is null.");
        }
    }
}
