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
package com.apicatalog.jsonld.processor;

import java.io.IOException;
import java.net.URI;
import java.util.Collection;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdOptions;
import com.apicatalog.jsonld.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.JsonLdVersion;
import com.apicatalog.jsonld.api.CommonApi;
import com.apicatalog.jsonld.api.ContextApi;
import com.apicatalog.jsonld.api.LoaderApi;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.tordf.JsonLdToQuads;
import com.apicatalog.rdf.api.RdfQuadConsumer;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class RdfEmitter implements
        CommonApi<RdfEmitter>,
        ContextApi<RdfEmitter>,
        LoaderApi<RdfEmitter> {

    private JsonLdOptions options;

    public RdfEmitter() {
        this(new JsonLdOptions());
    }

    public RdfEmitter(JsonLdOptions options) {
        this.options = options;
    }

    @Override
    public RdfEmitter options(JsonLdOptions options) {

        if (options == null) {
            throw new IllegalArgumentException("Parameter 'options' is null.");
        }

        this.options = options;
        return this;
    }

    @Override
    public RdfEmitter context(URI contextUri) {
        options.setExpandContext(contextUri);
        return this;
    }

//    @Override
//    public RdfEmitter context(String contextLocation) {
//
//        URI contextUri = null;
//
//        if (contextLocation != null) {
//
//            contextUri = UriUtils.create(contextLocation);
//
//            if (contextUri == null) {
//                throw new IllegalArgumentException("Context location must be valid URI or null but is [" + contextLocation + ".");
//            }
//        }
//
//        return context(contextUri);
//    }

//    @Override
//    public RdfEmitter context(JsonStructure context) {
//        options.expandContext(context != null ? JsonDocument.of(context) : null);
//        return this;
//    }

    @Override
    public RdfEmitter context(Document context) {
        options.expandContext(context);
        return this;
    }

    /**
     * If set to true, the JSON-LD processor may emit blank nodes for triple
     * predicates, otherwise they will be omitted.
     * 
     * @param enable {@code true} to produce generalized RDF
     * @return builder instance
     */
    public RdfEmitter produceGeneralizedRdf(boolean enable) {
        options.setProduceGeneralizedRdf(enable);
        return this;
    }

    /**
     * The JSON-LD processor may emit blank nodes for triple predicates.
     *
     * @return builder instance
     */
    public RdfEmitter produceGeneralizedRdf() {
        return produceGeneralizedRdf(true);
    }

    /**
     * Determines how value objects containing a base direction are transformed to
     * and from RDF.
     *
     * @param direction determines how to process directed language-tagged literals
     * @return builder instance
     */
    public RdfEmitter rdfDirection(RdfDirection direction) {
        options.setRdfDirection(direction);
        return this;
    }

    @Override
    public RdfEmitter mode(JsonLdVersion processingMode) {
        options.setProcessingMode(processingMode);
        return this;
    }

    @Override
    public RdfEmitter base(URI baseUri) {
        options.base(baseUri);
        return this;
    }

    @Override
    public RdfEmitter loader(DocumentLoader loader) {
        options.loader(loader);
        return this;
    }

    @Override
    public RdfEmitter ordered(boolean enable) {
        options.setOrdered(enable);
        return this;
    }

    /**
     * Experimental: Accept numeric <code>@id</code>. Disabled by default.
     *
     * @return builder instance
     */
    public RdfEmitter numericId() {
        options.setNumericId(true);
        return this;
    }

    /**
     * Emit transformed <code>JSON-LD</code> as RDF quads.
     * <p>
     * <strong>Note:</strong> toRdf() method adopts the {@link RdfQuadConsumer}
     * interface, allowing integration with Jena, Jelly, RDF4J, and other RDF
     * libraries.
     * </p>
     * 
     * @param document
     * @param consumer that accepts emitted RDF statements
     * @throws JsonLdException if the document transformation fails
     * @throws IOException
     */
    public void provide(Document document, RdfQuadConsumer consumer) throws JsonLdException, IOException {
        RdfEmitter.toRdf(document, consumer, options);
    }

    public static final void toRdf(final Document input, final RdfQuadConsumer consumer, final JsonLdOptions options) throws JsonLdException, IOException {
        final JsonLdOptions expansionOptions = new JsonLdOptions(options);

        expansionOptions.setProcessingMode(options.getProcessingMode());
        expansionOptions.base(options.getBase());
        expansionOptions.expandContext(options.getExpandContext());

        //FIXME
        final Execution runtime = Execution.of(options);
        runtime.tick();

        
        final var expanded = Expander.expand(input, expansionOptions, runtime);

        toRdf(expanded, consumer, options);
    }

    public static final void toRdf(final Collection<?> expanded, final RdfQuadConsumer consumer, final JsonLdOptions options) throws JsonLdException {
        JsonLdToQuads
                .with(new NodeMapBuilder(expanded, new NodeMap()).build())
                .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                .rdfDirection(options.getRdfDirection())
                .uriValidation(options.getUriValidation())
                .provide(consumer);
    }
}
