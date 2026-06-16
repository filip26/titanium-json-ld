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

import java.util.Collection;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.runtime.Execution;
import com.apicatalog.jsonld.tordf.JsonLdToQuads;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.Tree;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class RdfEmitter {

    private RdfEmitter() {
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
     */
    public void provide(
            Document document, 
            RdfQuadConsumer consumer,
            final Options options,
            final Execution runtime) throws JsonLdException {

        RdfEmitter.toRdf(document, consumer, options, runtime);
    }

    public static final void toRdf(
            final Document input, 
            final RdfQuadConsumer consumer, 
            final Options options,
            final Execution runtime) throws JsonLdException {
        final Options expansionOptions = Options.copyOf(options);

        expansionOptions.mode(options.mode());
        expansionOptions.base(options.base());
        expansionOptions.expandContext(options.expandContext());

        final var expanded = Expander.expand(input, expansionOptions, runtime);

        toRdf(expanded, consumer, options, runtime);
    }

    public static final void toRdf(
            final Tree input,
            final RdfQuadConsumer consumer,
            final Options options,
            final Execution runtime) throws JsonLdException {
        
        final Options expansionOptions = Options.copyOf(options);

        expansionOptions.mode(options.mode());
        expansionOptions.base(options.base());
        expansionOptions.expandContext(options.expandContext());

        final var expanded = Expander.expand(
                input,
                Expander.context(null, null, options, runtime),
                options.base(),
                expansionOptions,
                runtime);

        toRdf(expanded, consumer, options, runtime);
    }

    public static final void toRdf(
            final Collection<?> expanded,
            final RdfQuadConsumer consumer,
            final Options options,
            final Execution runtime) throws JsonLdException {

        JsonLdToQuads
                .with(new NodeMapBuilder(expanded, new NodeMap()).build())
                .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                .rdfJsonLiteralWriter(options.rdfJsonLiteralWriter())
                .rdfDirection(options.rdfDirection())
                .uriValidation(options.uriValidation())
                .provide(consumer);
    }
}
