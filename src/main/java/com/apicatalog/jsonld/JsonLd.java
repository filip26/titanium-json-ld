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
import java.util.Collection;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.framing.Frame;
import com.apicatalog.jsonld.fromrdf.QuadsToJsonLd;
import com.apicatalog.jsonld.processor.Compactor;
import com.apicatalog.jsonld.processor.Expander;
import com.apicatalog.jsonld.processor.Flattener;
import com.apicatalog.jsonld.processor.Framer;
import com.apicatalog.jsonld.processor.RdfEmitter;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 * The {@code JsonLd} class provides high-level static methods to process
 * JSON-LD data according to the
 * <a href="https://www.w3.org/TR/json-ld11-api/">JSON-LD 1.1 Processing
 * API</a>.
 *
 * <p>
 * It supports expansion, compaction, flattening, framing, and conversion
 * between RDF and JSON-LD representations.
 * </p>
 *
 * <p>
 * All methods are thread-safe and stateless.
 * </p>
 */
public final class JsonLd {

    private JsonLd() {
    }

    /* --- EXPAND -- */

    /**
     * Expands the referenced document. DocumentLoader must be set in order to fetch
     * the referenced document. See JsonLdOptions.loader method.
     *
     * @param document {@link URI} referencing JSON-LD document to expand
     * @param options  options to configure expansion
     * @return expanded JSON-LD document
     * @throws JsonLdException if expansion fails
     */
    public static final Collection<?> expand(
            final URI document,
            final Options options) throws JsonLdException {

        return Expander.expand(
                Document.load(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                options,
                Execution.of(options));
    }

    /**
     * Expands the provided remote document.
     *
     * @param document to expand
     * @param options  options to configure expansion
     * @return expanded JSON-LD document
     * @throws JsonLdException if expansion fails
     */
    public static final Collection<?> expand(
            final Document document,
            final Options options) throws JsonLdException {

        return Expander.expand(
                document,
                options,
                Execution.of(options));
    }

    /**
     * Expands a JSON-LD document provided as a {@link Map}.
     *
     * @param document a {@link Map} representation of the JSON-LD document to
     *                 expand
     * @param options  options to configure expansion
     * @return the expanded JSON-LD document
     * @throws JsonLdException if expansion fails
     */
    public static final Collection<?> expand(
            final Map<String, ?> document,
            final Options options) throws JsonLdException {

        return expand(new TreeIO(document, NativeAdapter.instance()), options);
    }

    /**
     * Expands a JSON-LD document provided as a {@link TreeIO} node.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 expand
     * @param options  options to configure expansion
     * @return the expanded JSON-LD document
     * @throws JsonLdException if expansion fails
     */
    public static final Collection<?> expand(
            final TreeIO document,
            final Options options) throws JsonLdException {

        return Expander.expand(
                document,
                options,
                Execution.of(options));
    }

    /* --- COMPACT -- */

    /**
     * Compacts the referenced document using the context.
     *
     * @param document {@link URI} referencing JSON-LD document to compact
     * @param context  {@link URI} referencing the context to use when compacting
     *                 the document
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final URI document,
            final URI context,
            final Options options) throws JsonLdException {
        return compact(
                Document.load(document, options.loader(), options.isExtractAllScripts()),
                Context.load(context, options.loader()),
                options);
    }

    /**
     * Compacts the referenced document using the context.
     *
     * @param document {@link URI} referencing JSON-LD document to compact
     * @param context  {@link Document} representing the context to use when
     *                 compacting the document
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final URI document,
            final Document context,
            final Options options) throws JsonLdException {
        return compact(
                Document.load(document, options.loader(), options.isExtractAllScripts()),
                context,
                options);
    }

    /**
     * Compacts a {@link Document} using the given context.
     *
     * @param document {@link Document} to compact
     * @param context  {@link URI} referencing the context to use when compacting
     *                 the document
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final Document document,
            final URI context, Options options) throws JsonLdException {
        return compact(
                document,
                Context.load(context, options.loader()),
                options);
    }

    /**
     * Compacts a {@link Document} using the given context.
     *
     * @param document {@link Document} to compact
     * @param context  {@link Document} representing the context
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final Document document,
            final Document context,
            final Options options) throws JsonLdException {

        return Compactor.compact(
                document,
                context,
                options,
                Execution.of(options));
    }

    /**
     * Compacts a JSON-LD document and context provided as {@link TreeIO} node.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 compact
     * @param context  a {@link TreeIO} representation of the context to use
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final TreeIO document,
            final TreeIO context,
            final Options options) throws JsonLdException {

        final Execution runtime = Execution.of(options);

        return Compactor.compact(
                Compactor.expand(document, options, runtime),
                null,
                context,
                options,
                runtime);
    }

    /**
     * Compacts a JSON-LD document provided as a {@link Collection} using a context
     * provided as a {@link Map}.
     *
     * @param document a {@link Collection} representing the JSON-LD document to
     *                 compact
     * @param context  a {@link Map} representation of the context to use
     * @param options  options to configure compaction
     * @return the compacted JSON-LD document
     * @throws JsonLdException if compaction fails
     */
    public static final Map<String, ?> compact(
            final Collection<?> document,
            final Map<String, ?> context,
            final Options options) throws JsonLdException {

        return compact(
                new TreeIO(document, NativeAdapter.instance()),
                new TreeIO(context, NativeAdapter.instance()),
                options);
    }

    /* --- FLATTEN -- */

    /**
     * Flattens the referenced JSON-LD document.
     *
     * @param document a {@link URI} referencing the JSON-LD document to flatten
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final URI document,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                Document.load(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                null,
                options,
                Execution.of(options));
    }

    /**
     * Flattens the given input and compacts it using a context.
     *
     * @param document {@link URI} referencing JSON-LD document to flatten
     * @param context  {@link URI} referencing the context to use when flattening
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final URI document,
            final URI context,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                Document.load(
                        document,
                        options.loader(),
                        options.isExtractAllScripts()),
                context != null
                        ? Context.load(context, options.loader()).content()
                        : null,
                options,
                Execution.of(options));
    }

    /**
     * Flattens a JSON-LD document.
     *
     * @param document a {@link Document} representing the JSON-LD document to
     *                 flatten
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final Document document,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                document,
                null,
                options,
                Execution.of(options));
    }

    /**
     * Flattens the remote input and optionally compacts it using context.
     *
     * @param document to flatten
     * @throws JsonLdException
     */
    public static final Object flatten(
            final Document document,
            final Document context,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                document,
                context != null
                        ? context.content()
                        : null,
                options,
                Execution.of(options));
    }

    /**
     * Flattens {@code Map} representing JSON-LD document.
     *
     * @param document a {@link Map} representation of the JSON-LD document to
     *                 flatten
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final Map<String, ?> document,
            final Options options) throws JsonLdException {

        return flatten(
                new TreeIO(document, NativeAdapter.instance()),
                null,
                options);
    }

    /**
     * Flattens JSON-LD document and applies a context, both provided as
     * {@link Map}.
     *
     * @param document a {@link Map} representation of the JSON-LD document to
     *                 flatten
     * @param context  a {@link Map} representation of the context to apply
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final Map<String, ?> document,
            final Map<String, ?> context,
            final Options options) throws JsonLdException {

        return flatten(
                new TreeIO(document, NativeAdapter.instance()),
                context != null
                        ? new TreeIO(context, NativeAdapter.instance())
                        : null,
                options);
    }

    /**
     * Flattens a JSON-LD document provided as a {@link Collection}.
     *
     * @param document a {@link Collection} representing the JSON-LD document to
     *                 flatten
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final Collection<?> document,
            final Options options) throws JsonLdException {

        return flatten(
                new TreeIO(document, NativeAdapter.instance()),
                null,
                options);
    }

    /**
     * Flattens a JSON-LD document provided as a {@link Collection} and applies a
     * context.
     *
     * @param document a {@link Collection} representing the JSON-LD document to
     *                 flatten
     * @param context  a {@link Map} representation of the context to apply
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final Collection<?> document,
            final Map<String, ?> context,
            final Options options) throws JsonLdException {

        return flatten(
                new TreeIO(document, NativeAdapter.instance()),
                context != null
                        ? new TreeIO(context, NativeAdapter.instance())
                        : null,
                options);
    }

    /**
     * Flattens a JSON-LD document represented as a {@link TreeIO} node.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 flatten
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final TreeIO document,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                document,
                null,
                options,
                Execution.of(options));
    }

    /**
     * Flattens a JSON-LD document represented as a {@link TreeIO} node.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 flatten
     * @param context  a {@link TreeIO} representation of the context to apply
     *                 during flattening
     * @param options  options to configure flattening
     * @return the flattened JSON-LD document
     * @throws JsonLdException if flattening fails
     */
    public static final Object flatten(
            final TreeIO document,
            final TreeIO context,
            final Options options) throws JsonLdException {

        return Flattener.flatten(
                document,
                context,
                options,
                Execution.of(options));
    }

    /* --- FRAME -- */

    /**
     * Frames the given remote JSON-LD document using a remote frame.
     *
     * @param document {@link URI} referencing the JSON-LD document to frame
     * @param frame    {@link URI} referencing the JSON-LD frame
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final URI document,
            final URI frame,
            final Options options) throws JsonLdException {

        return frame(
                Document.load(document, options.loader(), options.isExtractAllScripts()),
                frame,
                options);
    }

    /**
     * Frames the remote JSON-LD document using the given remote frame.
     *
     * @param document {@link Document} representing the JSON-LD document to frame
     * @param frame    {@link URI} referencing the JSON-LD frame
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final Document document,
            final URI frame,
            final Options options) throws JsonLdException {

        return frame(
                document,
                Document.load(frame, options.loader(), options.isExtractAllScripts()),
                options);
    }

    /**
     * Frames the remote JSON-LD document using the given local frame.
     *
     * @param document {@link URI} referencing the JSON-LD document to frame
     * @param frame    {@link Document} representing the JSON-LD frame
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final URI document,
            final Document frame,
            final Options options) throws JsonLdException {

        return frame(
                Document.load(document, options.loader(), options.isExtractAllScripts()),
                frame,
                options);
    }

    /**
     * Frames the local JSON-LD document using the given local frame.
     *
     * @param document {@link Document} to frame
     * @param frame    {@link Document} representing the JSON-LD frame
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final Document document,
            final Document frame,
            final Options options) throws JsonLdException {

        return Framer.frame(
                document,
                frame,
                options,
                Execution.of(options));
    }

    /**
     * Frames a JSON-LD document and frame provided as {@link Map} representations.
     *
     * @param document a {@link Map} representation of the JSON-LD document to frame
     * @param frame    a {@link Map} representation of the JSON-LD frame to apply
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final Map<String, ?> document,
            final Map<String, ?> frame,
            final Options options) throws JsonLdException {

        return frame(
                new TreeIO(document, NativeAdapter.instance()),
                new TreeIO(frame, NativeAdapter.instance()),
                options);
    }

    /**
     * Frames a JSON-LD document and frame provided as {@link TreeIO} node.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 frame
     * @param frame    a {@link TreeIO} representation of the JSON-LD frame to apply
     * @param options  options to configure framing
     * @return the framed JSON-LD document
     * @throws JsonLdException if framing fails
     */
    public static final Map<String, ?> frame(
            final TreeIO document,
            final TreeIO frame,
            final Options options) throws JsonLdException {

        final Execution runtime = Execution.of(options);

        final var contextNode = Context.extract(frame);

        return Context.inject(
                Framer.frame(
                        Framer.expand(document, options, runtime),
                        Frame.of(frame, options, runtime),
                        Framer.context(
                                null,
                                contextNode,
                                options.base(),
                                options,
                                runtime),
                        options),
                contextNode);
    }

    /* --- TO RDF -- */

    /**
     * Transforms the given input into RDF.
     *
     * @param uri      {@code URI} referencing JSON-LD document to transform
     * @param consumer {@link RdfQuadConsumer} receiving emitted RDF quads
     * @param options  options to configure transformation
     * @throws JsonLdException if transformation fails
     * 
     */
    public static final void toRdf(
            final URI uri,
            RdfQuadConsumer consumer,
            Options options) throws JsonLdException {
        toRdf(Document
                .load(
                        uri,
                        options.loader(),
                        options.isExtractAllScripts()),
                consumer,
                options);
    }

    /**
     * Transforms the given JSON-LD document into RDF quads.
     *
     * @param document {@link URI} referencing the JSON-LD document to transform
     * @param consumer {@link RdfQuadConsumer} receiving emitted RDF quads
     * @param options  options to configure transformation
     * @throws JsonLdException if transformation fails
     */
    public static final void toRdf(
            final Document document,
            final RdfQuadConsumer consumer,
            final Options options) throws JsonLdException {

        RdfEmitter.toRdf(
                document,
                consumer,
                options,
                Execution.of(options));
    }

    /**
     * Transforms a JSON-LD document represented as {@link TreeIO} node into RDF
     * quads.
     *
     * @param document a {@link TreeIO} representation of the JSON-LD document to
     *                 transform
     * @param consumer an {@link RdfQuadConsumer} receiving emitted RDF quads
     * @param options  options to configure transformation
     * @throws JsonLdException if transformation fails
     */
    public static final void toRdf(
            final TreeIO document,
            final RdfQuadConsumer consumer,
            final Options options) throws JsonLdException {

        RdfEmitter.toRdf(
                document,
                consumer,
                options,
                Execution.of(options));
    }

    /**
     * Transforms a JSON-LD document represented as a {@link Map} into RDF quads.
     *
     * @param document a {@link Map} representation of the JSON-LD document to
     *                 transform
     * @param consumer an {@link RdfQuadConsumer} receiving emitted RDF quads
     * @param options  options to configure transformation
     * @throws JsonLdException if transformation fails
     */
    public static final void toRdf(
            final Map<String, ?> document,
            final RdfQuadConsumer consumer,
            final Options options) throws JsonLdException {

        RdfEmitter.toRdf(
                new TreeIO(document, NativeAdapter.instance()),
                consumer,
                options,
                Execution.of(options));
    }

    /* --- FROM RDF -- */

    /**
     * Transforms an RDF quad set into a JSON-LD document in expanded form.
     * <p>
     * Use <a href="https://github.com/filip26/titanium-rdf-n-quads">Titanium RDF
     * N-QUADS</a> or any 3rd party library to read quads, or manually add them to
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

    /**
     * Transforms an RDF quad set into a JSON-LD document in expanded form.
     * <p>
     * Use <a href="https://github.com/filip26/titanium-rdf-n-quads">Titanium RDF
     * N-QUADS</a> or any 3rd party library to read quads, or manually add them to
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
     * @since 2.0.0
     *
     * @param options
     * @return a {@link QuadsToJsonLd} instance, allowing additional parameter
     *         configuration
     */
    public static final QuadsToJsonLd fromRdf(Options options) {
        return new QuadsToJsonLd().options(options);
    }

    /**
     * Represents supported JSON-LD versions.
     */
    public enum Version {

        V1_0("json-ld-1.0"), V1_1("json-ld-1.1");

        private final String text;

        Version(final String text) {
            this.text = text;
        }

        /**
         * Gets {@link Version} from a string value.
         *
         * @param version textual representation of the JSON-LD version
         * @return the corresponding {@link Version}, or {@code null} if unrecognized
         * @throws NullPointerException if {@code version} is null
         */
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

        /**
         * Returns the string form of this version.
         *
         * @return the JSON-LD version identifier
         */
        @Override
        public String toString() {
            return text;
        }
    }
}
