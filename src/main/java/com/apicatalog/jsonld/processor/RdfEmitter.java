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
import java.util.Collection;

import com.apicatalog.jsonld.Document;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.flattening.NodeMapBuilder;
import com.apicatalog.jsonld.tordf.JsonLdToQuads;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.tree.io.TreeIO;

/**
 *
 * @see <a href=
 *      "https://w3c.github.io/json-ld-api/#dom-jsonldprocessor-tordf">JsonLdProcessor.toRdf()</a>
 *
 */
public final class RdfEmitter {

    private final Options options;

//    public RdfEmitter() {
//        this(new JsonLdOptions());
//    }

    public RdfEmitter(Options options) {
        this.options = options;
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

    public static final void toRdf(final Document input, final RdfQuadConsumer consumer, final Options options) throws JsonLdException, IOException {
        final Options expansionOptions = Options.copyOf(options);

        expansionOptions.mode(options.mode());
        expansionOptions.base(options.base());
        expansionOptions.expandContext(options.expandContext());

        // FIXME
        final Execution runtime = Execution.of(options);
        runtime.tick();

        final var expanded = Expander.expand(input, expansionOptions, runtime);

        toRdf(expanded, consumer, options);
    }

    public static final void toRdf(final TreeIO input, final RdfQuadConsumer consumer, final Options options) throws JsonLdException, IOException {
        final Options expansionOptions = Options.copyOf(options);

        expansionOptions.mode(options.mode());
        expansionOptions.base(options.base());
        expansionOptions.expandContext(options.expandContext());

        // FIXME
        final Execution runtime = Execution.of(options);
        runtime.tick();

        final var expanded = Expander.expand(input, Expander.context(null, null, options), options.base(), expansionOptions, runtime);

        toRdf(expanded, consumer, options);
    }

    public static final void toRdf(final Collection<?> expanded, final RdfQuadConsumer consumer, final Options options) throws JsonLdException {
        JsonLdToQuads
                .with(new NodeMapBuilder(expanded, new NodeMap()).build())
                .produceGeneralizedRdf(options.isProduceGeneralizedRdf())
                .jsonWriter(options.rdfJsonLiteralWriter())
                .rdfDirection(options.rdfDirection())
                .uriValidation(options.uriValidation())
                .provide(consumer);
    }
}
