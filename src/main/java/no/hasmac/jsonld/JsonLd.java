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

import java.net.URI;

import no.hasmac.jsonld.api.CompactionApi;
import no.hasmac.jsonld.api.ExpansionApi;
import no.hasmac.jsonld.api.FlatteningApi;
import no.hasmac.jsonld.api.FramingApi;
import no.hasmac.jsonld.api.FromRdfApi;
import no.hasmac.jsonld.api.ToRdfApi;
import no.hasmac.jsonld.document.Document;
import no.hasmac.jsonld.uri.UriUtils;
import no.hasmac.rdf.RdfDataset;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;

/**
 * The {@link JsonLd} interface is the high-level programming structure
 * that developers use to access the JSON-LD transformation methods.
 * This class provides methods to process JSON-LD.
 *
 * All the methods in this class are thread-safe.
 */
public final class JsonLd {

    private static final String DOCUMENT_LOCATION_PARAM_NAME = "documentLocation";
    private static final String DOCUMENT_URI_PARAM_NAME = "documentUri";
    private static final String DOCUMENT_PARAM_NAME = "document";
    private static final String CONTEXT_PARAM_NAME = "context";
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
    public static ExpansionApi expand(final String documentLocation) {
        return new ExpansionApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Expands the referenced document.
     *
     * @param documentUri {@link URI} referencing JSON-LD document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters
     */
    public static ExpansionApi expand(final URI documentUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new ExpansionApi(documentUri);
    }

    /**
     * Expands the provided remote document.
     *
     * @param document to expand
     * @return {@link ExpansionApi} allowing to set additional parameters
     */
    public static ExpansionApi expand(final Document document) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);

        return new ExpansionApi(document);
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param contextLocation {@code IRI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final String documentLocation, final String contextLocation) {
        return compact(
                assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME),
                assertLocation(contextLocation, "contextLocation")
                );
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentUri {@link URI} referencing JSON-LD document to compact
     * @param contextUri {@link URI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final URI documentUri, final URI contextUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(contextUri, "contextUri");

        return new CompactionApi(documentUri, contextUri);
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param context {@link Document} representing the context or {@link JsonArray} consisting of {@link JsonObject} and {@link JsonString} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final String documentLocation, final Document context) {

        assertJsonDocument(context, CONTEXT_PARAM_NAME);

        return new CompactionApi(
                assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME),
                context
                );
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to compact
     * @param context {@link Document} representing the context or {@link JsonArray} consisting of one or many {@link JsonObject} and {@link JsonString} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final URI documentUri, final Document context) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertJsonDocument(context, CONTEXT_PARAM_NAME);

        return new CompactionApi(documentUri, context);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param context JSON-LD document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final Document document, final Document context) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);
        assertJsonDocument(context, CONTEXT_PARAM_NAME);

        return new CompactionApi(document, context);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param contextLocation {@code IRI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final Document document, final String contextLocation) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);

        return compact(document, assertLocation(contextLocation, "contextLocation"));
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param contextUri {@link URI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final Document document, final URI contextUri) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);
        assertUri(contextUri, "contextUri");

        return new CompactionApi(document, contextUri);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to compact
     * @param contextUri {@link URI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final String documentLocation, final URI contextUri) {

        assertUri(contextUri, "contextUri");

        return new CompactionApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME), contextUri);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to compact
     * @param contextLocation {@code IRI} referencing the context to use when compacting the document
     * @return {@link CompactionApi} allowing to set additional parameters
     */
    public static CompactionApi compact(final URI documentUri, final String contextLocation) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new CompactionApi(documentUri, assertLocation(contextLocation, "contextLocation"));
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static FlatteningApi flatten(final String documentLocation) {
        return new FlatteningApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Flattens the given input and optionally compacts it using context.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static FlatteningApi flatten(final URI documentUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new FlatteningApi(documentUri);
    }

    /**
     * Flattens the remote input and optionally compacts it using context.
     *
     * @param document to flatten
     * @return {@link FlatteningApi} allowing to set additional parameters
     */
    public static FlatteningApi flatten(final Document document) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);

        return new FlatteningApi(document);
    }

    /**
     *  Frames the given remote input using remote frame.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frameUri {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final URI documentUri, final URI frameUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertUri(frameUri, FRAME_URI_PARAM_NAME);

        return new FramingApi(documentUri, frameUri);
    }

    /**
     *  Frames the given remote input using remote frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frameLocation {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final String documentLocation, final String frameLocation) {
        return new FramingApi(
                assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME),
                assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME)
                );
    }

    /**
     *  Frames the local document using given local frame.
     *
     * @param document to frame
     * @param frame JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final Document document, final Document frame) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);
        assertJsonDocument(frame, FRAME_PARAM_NAME);

        return new FramingApi(document, frame);
    }

    /**
     *  Frames the local document using given remote frame.
     *
     * @param document to frame
     * @param frameLocation {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final Document document, final String frameLocation) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);

        return new FramingApi(document, assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME));
    }

    /**
     *  Frames the local document using given remote frame.
     *
     * @param document to frame
     * @param frameUri {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final Document document, final URI frameUri) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);
        assertUri(frameUri, FRAME_URI_PARAM_NAME);

        return new FramingApi(document, frameUri);
    }

    /**
     *  Frames the remote input using given local frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frame JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final String documentLocation, final Document frame) {

        assertJsonDocument(frame, FRAME_PARAM_NAME);

        return new FramingApi(assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME), frame);
    }

    /**
     *  Frames the remote input using given remote frame.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to frame
     * @param frameUri {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final String documentLocation, final URI frameUri) {

        assertUri(frameUri, FRAME_URI_PARAM_NAME);

        return new FramingApi(assertLocation(documentLocation, DOCUMENT_URI_PARAM_NAME), frameUri);
    }

    /**
     *  Frames the remote input using given local frame.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frame JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final URI documentUri, final Document frame) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);
        assertJsonDocument(frame, FRAME_PARAM_NAME);

        return new FramingApi(documentUri, frame);
    }

    /**
     *  Frames the remote input using given remote frame.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to frame
     * @param frameLocation {@code IRI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static FramingApi frame(final URI documentUri, final String frameLocation) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new FramingApi(documentUri, assertLocation(frameLocation, FRAME_LOCATION_PARAM_NAME));
    }

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentLocation {@code IRI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static ToRdfApi toRdf(final String documentLocation) {
        return new ToRdfApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static ToRdfApi toRdf(final URI documentUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new ToRdfApi(documentUri);
    }

    /**
     * Transforms {@link Document} into {@link RdfDataset}.
     *
     * @param document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static ToRdfApi toRdf(final Document document) {

        assertJsonDocument(document, DOCUMENT_PARAM_NAME);

        return new ToRdfApi(document);
    }

    /**
     * Transforms the referenced N-Quads document into a JSON-LD document in expanded form.
     *
     * @param documentLocation {@link URI} referencing N-Quads document to expand
     * @return {@link FromRdfApi} allowing to set additional parameters
     */
    public static FromRdfApi fromRdf(final String documentLocation) {
        return new FromRdfApi(assertLocation(documentLocation, DOCUMENT_LOCATION_PARAM_NAME));
    }

    /**
     * Transforms the referenced N-Quads document into a JSON-LD document in expanded form.
     *
     * @param documentUri {@link URI} referencing N-Quads document to expand
     * @return {@link FromRdfApi} allowing to set additional parameters
     */
    public static FromRdfApi fromRdf(final URI documentUri) {

        assertUri(documentUri, DOCUMENT_URI_PARAM_NAME);

        return new FromRdfApi(documentUri);
    }

    /**
     * Transforms {@link Document} into a JSON-LD document in expanded form.
     *
     * @param document to transform
     * @return {@link FromRdfApi} allowing to set additional parameters
     */
    public static FromRdfApi fromRdf(final Document document) {

        assertRdfDocument(document, DOCUMENT_PARAM_NAME);

        return new FromRdfApi(document);
    }

    private static URI assertLocation(final String location, final String param) {

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

    private static void assertUri(final URI uri, final String param) {

        assertNotNull(uri, param);

        if (!uri.isAbsolute()) {
            throw new IllegalArgumentException("'" + param + "' is not an absolute URI [" + uri + "].");
        }
    }

    private static void assertJsonDocument(final Document document, final String param) {

        assertNotNull(document, param);

        if (!document.getJsonContent().isPresent()) {
            throw new IllegalArgumentException("'" + param + "' is not not JSON document but [" + document.getContentType() + "].");
        }
    }

    private static void assertRdfDocument(final Document document, final String param) {

        assertNotNull(document, param);

        if (!document.getRdfContent().isPresent()) {
            throw new IllegalArgumentException("'" + param + "' is not not RDF document but [" + document.getContentType() + "].");
        }
    }

    private static void assertNotNull(Object value, final String param) {
        if (value == null) {
            throw new IllegalArgumentException("'" + param + "' is null.");
        }
    }
}
