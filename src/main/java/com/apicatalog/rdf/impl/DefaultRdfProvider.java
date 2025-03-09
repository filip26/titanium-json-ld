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
package com.apicatalog.rdf.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;

import com.apicatalog.jsonld.StringUtils;
import com.apicatalog.jsonld.http.media.MediaType;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedContentException;
import com.apicatalog.rdf.io.nquad.NQuadsReader;
import com.apicatalog.rdf.io.nquad.NQuadsWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class DefaultRdfProvider extends RdfProvider {

    public static final RdfProvider INSTANCE = new DefaultRdfProvider();

    private static final Collection<MediaType> CAN_READWRITE = Arrays.asList(MediaType.N_QUADS);

    @Override
    public RdfDataset createDataset() {
        return new MutableRdfDataset();
    }

    @Override
    public RdfReader createReader(final MediaType contentType, final Reader reader) throws UnsupportedContentException {

        if (reader == null || contentType == null) {
            throw new IllegalArgumentException();
        }

        if (MediaType.N_QUADS.match(contentType)) {
            return new NQuadsReader(reader);
        }
        throw new UnsupportedContentException(contentType.toString());
    }

    @Override
    public RdfWriter createWriter(final MediaType contentType, final Writer writer) throws UnsupportedContentException {

        if (writer == null || contentType == null) {
            throw new IllegalArgumentException();
        }

        if (MediaType.N_QUADS.match(contentType)) {
            return new NQuadsWriter(writer);
        }

        throw new UnsupportedContentException(contentType.toString());
    }

    @Override
    public RdfGraph createGraph() {
        return new MutableRdfGraph();
    }

    @Override
    public RdfTriple createTriple(RdfResource subject, RdfResource predicate, RdfValue object) {

        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return new ImmutableRdfTriple(subject, predicate, object);
    }

    @Override
    public RdfNQuad createNQuad(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName) {

        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return new ImmutableRdfNQuad(subject, predicate, object, graphName);
    }

    @Override
    public RdfResource createBlankNode(String value) {
        if (value == null || isBlank(value)) {
            throw new IllegalArgumentException();
        }

        if (!value.startsWith("_:")) {
            return new ImmutableRdfResource("_:" +value, true);
        }

        return new ImmutableRdfResource(value, true);
    }

    @Override
    public RdfResource createIRI(String value) {
        if (value == null || isBlank(value)) {
            throw new IllegalArgumentException();
        }

        return new ImmutableRdfResource(value, false);
    }

    @Override
    public RdfLiteral createLangString(String lexicalForm, String langTag) {
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }

        return new ImmutableRdfLiteral(lexicalForm, langTag, null);
    }

    @Override
    public RdfLiteral createTypedString(String lexicalForm, String datatype) {
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }

        return new ImmutableRdfLiteral(lexicalForm, null, datatype);
    }

    @Override
    public Collection<MediaType> canRead() {
        return CAN_READWRITE;
    }

    @Override
    public Collection<MediaType> canWrite() {
        return CAN_READWRITE;
    }

    private static final boolean isBlank(String value) {
        return value.isEmpty()
                || StringUtils.isBlank(value) && value.chars().noneMatch(ch -> ch == '\n' || ch == '\r' || ch == '\t' || ch == '\f');
    }
}