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
package no.hasmac.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;
import java.util.Optional;

import no.hasmac.rdf.RdfDataset;
import no.hasmac.rdf.RdfLiteral;
import no.hasmac.rdf.RdfNQuad;
import no.hasmac.rdf.RdfResource;
import no.hasmac.rdf.RdfValue;
import no.hasmac.rdf.io.RdfWriter;
import no.hasmac.rdf.lang.XsdConstants;

/**
 *
 * @see <a href="https://www.w3.org/TR/n-quads/">RDF 1.1. N-Quads</a>
 *
 */
public class NQuadsWriter implements RdfWriter {

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

        writeValue(nquad.getSubject());
        writer.write(' ');

        writeValue(nquad.getPredicate());
        writer.write(' ');

        writeValue(nquad.getObject());
        writer.write(' ');

        final Optional<RdfResource> graphName = nquad.getGraphName();

        if (graphName.isPresent()) {
            writeValue(graphName.get());
            writer.write(' ');
        }

        writer.write(".\n");
    }

    public void writeValue(RdfValue object) throws IOException {
        if (object == null) {
            throw new IllegalArgumentException();
        }

        if (object.isIRI()) {
            writeIri(object.toString());
            return;
        }

        if (object.isLiteral()) {
            write(object.asLiteral());
            return;
        }

        if (object.isBlankNode()) {
            writer.write(object.toString());
            return;
        }

        throw new IllegalStateException();
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

    public static String escape(String value) {

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
}
