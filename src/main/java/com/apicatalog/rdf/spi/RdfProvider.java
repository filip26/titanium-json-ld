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
package com.apicatalog.rdf.spi;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.impl.DefaultRdfProvider;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedContentException;

/**
 * This class is deprecated as of version 1.7.0.
 * <p>
 * Please use
 * <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium RDF
 * Primitives</a> or any other third-party library to materialize RDF
 * primitives.
 * </p>
 *
 * @see <a href="https://github.com/filip26/titanium-rdf-primitives">Titanium
 *      RDF Primitives</a>
 * @see <a href="https://github.com/filip26/titanium-rdf-n-quads">Titanium RDF
 *      N-QUADS</a>
 * @deprecated since 1.7.0 - use an alternative RDF primitives library.
 */
@Deprecated
public abstract class RdfProvider {

    private static RdfProvider provider = null;

    protected RdfProvider() {
    }

    public static final RdfProvider provider() {
        return provider != null ? provider : DefaultRdfProvider.INSTANCE;
    }

    public static final void setProvider(RdfProvider instance) {
        provider = instance;
    }

    public abstract RdfDataset createDataset();

    public abstract Collection<MediaType> canRead();

    public abstract RdfReader createReader(MediaType contentType, Reader reader) throws UnsupportedContentException;

    public abstract Collection<MediaType> canWrite();

    public abstract RdfWriter createWriter(MediaType contentType, Writer writer) throws UnsupportedContentException;

    public abstract RdfGraph createGraph();

    public abstract RdfTriple createTriple(RdfResource subject, RdfResource predicate, RdfValue object);

    public abstract RdfNQuad createNQuad(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName);

    public abstract RdfResource createBlankNode(String value);

    public abstract RdfResource createIRI(String value);

    public abstract RdfLiteral createLangString(String lexicalForm, String langTag);

    public abstract RdfLiteral createTypedString(String lexicalForm, String datatype);
}