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
import java.util.Objects;

import com.apicatalog.jsonld.api.FlatteningApi;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.fromrdf.QuadsToJsonLd;
import com.apicatalog.jsonld.processor.Compactor;
import com.apicatalog.jsonld.processor.Execution;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.processor.Framer;
import com.apicatalog.jsonld.processor.RdfEmitter;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 * The {@link JsonLd} interface is the high-level programming structure that
 * developers use to access the JSON-LD transformation methods. This class
 * provides methods to process JSON-LD.
 *
 * All the methods in this class are thread-safe.
 */
public final class JsonLd {

    public enum Version {

        V1_0("json-ld-1.0"), V1_1("json-ld-1.1");

        private final String text;

        Version(final String text) {
            this.text = text;
        }

        public static Version of(String version) {

            Objects.requireNonNull(version);

            if ("1.1".equals(version) || V1_1.text.equalsIgnoreCase(version)) {
                return V1_1;
            }

            if ("1.0".equals(version) || V1_0.text.equalsIgnoreCase(version)) {
                return V1_0;
            }
            return null;
        }

        @Override
        public String toString() {
            return text;
        }
    }

    private static final String DOCUMENT_URI_PARAM_NAME = "documentUri";
    private static final String DOCUMENT_PARAM_NAME = "document";

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
    public static final Collection<?> expand(final URI document, final Options options) throws JsonLdException, IOException {

        var runtime = Execution.of(options);
        runtime.tick();

        return Expander.expand(
                Document.fetch(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                options,
                runtime);
    }

    /**
     * Expands the provided remote document.
     *
     * @param document to expand
     * @param options
     * @return {@link ExpansionApi} allowing to set additional parameters
     * @throws JsonLdException
     */
    public static final Collection<?> expand(final Document document, final Options options) throws JsonLdException, IOException {

        var runtime = Execution.of(options);
        runtime.tick();

        return Expander.expand(document, options, runtime);
    }

    public static final Collection<?> expand(final Map<String, ?> document, final Options options) throws JsonLdException, IOException {

        if (!NativeAdapter.instance().isNode(document)) {
            throw new IllegalArgumentException();
        }

        return expand(new TreeIO(document, NativeAdapter.instance()), options);
    }

    public static final Collection<?> expand(final TreeIO document, final Options options) throws JsonLdException, IOException {

        var runtime = Execution.of(options);
        runtime.tick();

        return Expander.expand(
                document,
                Expander.context(null, null, options),
                Expander.baseUrl(null, options),
                options,
                runtime);
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
    public static final Map<String, ?> compact(
            final URI document,
            final URI context,
            final Options options) throws JsonLdException, IOException {
        return compact(
                Document.fetch(document, options.loader(), options.isExtractAllScripts()),
                Document.fetch(context, options.loader(), options.isExtractAllScripts()),
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
     * @param options
     * @return {@link CompactionApi} allowing to set additional parameters
     * @throws IOException
     * @throws JsonLdException
     */
    public static final Map<String, ?> compact(final URI document, final Document context,
            final Options options) throws JsonLdException, IOException {
        return compact(
                Document.fetch(document, options.loader(), options.isExtractAllScripts()),
                context,
                options);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param context  {@link URI} referencing the context to use when compacting
     *                 the document
     * @return {@link CompactionApi} allowing to set additional parameters
     * @throws IOException
     * @throws JsonLdException
     */
    public static final Map<String, ?> compact(final Document document, final URI context, Options options) throws JsonLdException, IOException {
        return compact(
                document,
                Document.fetch(context, options.loader(), options.isExtractAllScripts()),
                options);
    }

    /**
     * Compacts {@link Document} document using the context.
     *
     * @param document to compact
     * @param context  JSON-LD document
     * @param options
     * @return {@link CompactionApi} allowing to set additional parameters
     * @throws IOException
     * @throws JsonLdException
     */
    public static final Map<String, ?> compact(
            final Document document,
            final Document context,
            final Options options) throws JsonLdException, IOException {

        final Execution runtime = Execution.of(options);
        runtime.tick();

        return Compactor.compact(
                document,
                context,
                options,
                runtime);
    }

    public static final Map<String, ?> compact(
            final Collection<?> document,
            final Map<String, ?> context,
            final Options options) throws JsonLdException, IOException {

        // TODO
        return null;
    }

    /* --- FLATTEN -- */

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

    public static final Map<String, ?> flatten(final Map<String, ?> document, Options options) {
//        return new FlatteningApi(assertUri(documentUri, DOCUMENT_URI_PARAM_NAME));
        return null;
    }

    public static final Map<String, ?> flatten(final Collection<?> document, Options options) {
//      return new FlatteningApi(assertUri(documentUri, DOCUMENT_URI_PARAM_NAME));
        return null;
    }

    /* --- FRAME -- */

    /**
     * Frames the given remote input using remote frame.
     *
     * @param document {@code URI} referencing JSON-LD document to frame
     * @param frame    {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final URI document, final URI frame, final Options options) throws JsonLdException, IOException {
        return frame(
                Document.fetch(document, options.loader(), options.isExtractAllScripts()),
                frame,
                options);
    }

    /**
     * Frames the remote input using given remote frame.
     *
     * @param document {@code IRI} referencing JSON-LD document to frame
     * @param frame    {@code URI} referencing JSON-LD frame
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final Document document, final URI frame, final Options options) throws JsonLdException, IOException {
        return frame(
                document,
                Document.fetch(frame, options.loader(), options.isExtractAllScripts()),
                options);
    }

    /**
     * Frames the remote input using given local frame.
     *
     * @param document {@code URI} referencing JSON-LD document to frame
     * @param frame    JSON-LD definition
     * @return {@link FramingApi} allowing to set additional parameters
     */
    public static final Map<String, ?> frame(final URI document, final Document frame, final Options options) throws JsonLdException, IOException {
        return frame(
                Document.fetch(document, options.loader(), options.isExtractAllScripts()),
                frame,
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
    public static final Map<String, ?> frame(final Document document, final Document frame, final Options options) throws JsonLdException, IOException {

        final Execution runtime = Execution.of(options);
        runtime.tick();

        return Framer.frame(document, frame, options, runtime);
    }

    public static final Map<String, ?> frame(final TreeIO document, final TreeIO frame, final Options options) throws JsonLdException, IOException {
        final Execution runtime = Execution.of(options);
        runtime.tick();

        final var contextNode = Context.extract(frame);

        return Context.inject(
                Framer.frame(
                        Framer.expand(document, options, runtime),
                        Frame.of(frame, options, runtime),
                        Framer.context(
                                null,
                                contextNode,
                                options.base(),
                                options),
                        options),
                contextNode);
    }

    public static final Map<String, ?> frame(final Map<String, ?> document, final Map<String, ?> frame, final Options options) throws JsonLdException, IOException {

        if (!NativeAdapter.instance().isNode(document)) {
            throw new IllegalArgumentException();
        }
        if (!NativeAdapter.instance().isNode(frame)) {
            throw new IllegalArgumentException();
        }

        final Execution runtime = Execution.of(options);
        runtime.tick();

        return null;
        // TODO
//        return Framer.frame(
//                document, 
//                Frame.of(frame, options, runtime), 
//                options, 
//                runtime);
    }

    /* --- TO RDF -- */

    /**
     * Transforms the given input into {@link RdfDataset}.
     *
     * @param documentUri {@code URI} referencing JSON-LD document to transform
     * @return {@link ToRdfApi} allowing to set additional parameters
     */
    public static final void toRdf(final URI document, RdfQuadConsumer consumer, Options options) throws JsonLdException, IOException {
        toRdf(Document
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
    public static final void toRdf(final Document document, RdfQuadConsumer consumer, Options options) throws JsonLdException, IOException {
        RdfEmitter.toRdf(document, consumer, options);
    }

    public static final void toRdf(final TreeIO document, RdfQuadConsumer consumer, Options options) throws JsonLdException, IOException {
        RdfEmitter.toRdf(document, consumer, options);
    }

    public static final void toRdf(final Map<String, ?> document, RdfQuadConsumer consumer, Options options) throws JsonLdException, IOException {
        RdfEmitter.toRdf(new TreeIO(document, NativeAdapter.instance()), consumer, options);
    }

    /* --- FROM RDF -- */

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

    public static final QuadsToJsonLd fromRdf(Options options) {
        return new QuadsToJsonLd().options(options);
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

//        if (!document.getJsonContent().isPresent()) {
//            throw new IllegalArgumentException("'" + param + "' is not not JSON document but [" + document.contentType() + "].");
//        }
        return document;
    }

    private static final void assertNotNull(Object value, final String param) {
        if (value == null) {
            throw new IllegalArgumentException("'" + param + "' is null.");
        }
    }
}
