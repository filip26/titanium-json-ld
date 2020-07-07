package com.apicatalog.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;
import java.util.Optional;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfValue;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.lang.XsdConstants;

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
        
        if (nquad.getGraphName() != null) {
            writeValue(nquad.getGraphName());
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
}
