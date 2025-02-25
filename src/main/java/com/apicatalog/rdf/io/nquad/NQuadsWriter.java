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
package com.apicatalog.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;
import java.util.Optional;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.api.RdfConsumerException;
import com.apicatalog.rdf.api.RdfQuadConsumer;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.lang.XsdConstants;

/**
 *
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
@Deprecated
public class NQuadsWriter implements RdfWriter, RdfQuadConsumer {

    private final Writer writer;

    public NQuadsWriter(Writer writer) {
        this.writer = writer;
    }

    @Override
    public void write(final RdfDataset dataset) throws IOException {

        for (RdfNQuad nquad : dataset.toList()) {
            write(nquad);
        }

        writer.flush();
    }

    public void write(final RdfNQuad nquad) throws IOException {
        try {

            if (nquad.getObject().isLiteral()) {
                if (nquad.getObject().asLiteral().getLanguage().isPresent()) {
                    quad(nquad.getSubject().getValue(),
                            nquad.getPredicate().getValue(),
                            nquad.getObject().getValue(),
                            nquad.getObject().asLiteral().getLanguage().orElseThrow(),
                            null,
                            nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                    return;
                }
                String datatype = nquad.getObject().asLiteral().getDatatype();
                if (datatype.startsWith(RdfConstants.I18N_BASE)) {

                }

                quad(nquad.getSubject().getValue(),
                        nquad.getPredicate().getValue(),
                        nquad.getObject().getValue(),
                        datatype,
                        nquad.getGraphName().map(RdfResource::getValue).orElse(null));
                return;
            }

            quad(nquad.getSubject().getValue(),
                    nquad.getPredicate().getValue(),
                    nquad.getObject().getValue(),
                    nquad.getGraphName().map(RdfResource::getValue).orElse(null));
        } catch (RdfConsumerException e) {
            if (e.getCause() instanceof IOException) {
                throw (IOException) e.getCause();
            }
            throw new IllegalStateException(e);
        }
    }

    public void writeValue(RdfValue value) throws IOException {
        if (value == null) {
            throw new IllegalArgumentException();
        }

        if (value.isIRI()) {
            writeIri(value.getValue());
            return;
        }

        if (value.isLiteral()) {
            write(value.asLiteral());
            return;
        }

        if (value.isBlankNode()) {
            writer.write(value.getValue());
            return;
        }

        throw new IllegalStateException();
    }

    public void writeIriOrBlank(String value) throws IOException {
        if (value == null) {
            throw new IllegalArgumentException();
        }

        if (value.startsWith("_:")) {
            writer.write(value);
            return;
        }

        writeIri(value);
    }

    private void write(RdfLiteral literal) throws IOException {

        if (literal == null) {
            throw new IllegalArgumentException();
        }

        writer.write('"');
        writer.write(escape(literal.getValue()));
        writer.write('"');

        final Optional<String> language = literal.getLanguage();

        if (language.isPresent()) {

            writer.write("@");
            writer.write(language.get());

        } else if (literal.getDatatype() != null) {

            if (XsdConstants.STRING.equals(literal.getDatatype())) {
                return;
            }

            writer.write("^^");
            writeIri(literal.getDatatype());
        }
    }

    public static final String escape(String value) {

        final StringBuilder escaped = new StringBuilder();

        int[] codePoints = value.codePoints().toArray();

        for (int ch : codePoints) {

            if (ch == 0x9) {
                escaped.append("\\t");

            } else if (ch == 0x8) {
                escaped.append("\\b");

            } else if (ch == 0xa) {
                escaped.append("\\n");

            } else if (ch == 0xd) {
                escaped.append("\\r");

            } else if (ch == 0xc) {
                escaped.append("\\f");

            } else if (ch == '"') {
                escaped.append("\\\"");

            } else if (ch == '\\') {
                escaped.append("\\\\");

            } else if (ch >= 0x0 && ch <= 0x1f || ch == 0x7f) {
                escaped.append(String.format("\\u%04x", ch));

            } else {
                escaped.appendCodePoint(ch);
            }
        }
        return escaped.toString();
    }

    public void writeIri(String iri) throws IOException {

        if (iri == null) {
            throw new IllegalArgumentException();
        }

        writer.write('<');
        writer.write(iri);
        writer.write('>');
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String object, String graph) throws RdfConsumerException {
        try {
            writeIriOrBlank(subject);
            writer.write(' ');

            writeIriOrBlank(predicate);
            writer.write(' ');

            writeIriOrBlank(object);
            writer.write(' ');

            if (graph != null) {
                writeIriOrBlank(graph);
                writer.write(' ');
            }

            writer.write(".\n");
        } catch (IOException e) {
            throw new RdfConsumerException(e, subject, predicate, object, graph);
        }
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String datatype, String graph) throws RdfConsumerException {
        try {
            writeIriOrBlank(subject);
            writer.write(' ');

            writeIriOrBlank(predicate);
            writer.write(' ');

            
            writer.write(' ');

            if (graph != null) {
                writeIriOrBlank(graph);
                writer.write(' ');
            }

            writer.write(".\n");
        } catch (IOException e) {
            throw new RdfConsumerException(e, subject, predicate, literal, datatype, graph);
        }
        return this;
    }

    @Override
    public RdfQuadConsumer quad(String subject, String predicate, String literal, String language, String direction, String graph) {
        // TODO Auto-generated method stub
        return null;
    }
}
