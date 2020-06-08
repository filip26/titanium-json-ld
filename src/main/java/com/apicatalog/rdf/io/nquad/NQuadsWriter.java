package com.apicatalog.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.lang.XsdVocabulary;

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
        
        write(nquad.getSubject());
        writer.write(' ');
        
        write(nquad.getPredicate());
        writer.write(' ');
        
        write(nquad.getObject());
        writer.write(' ');
        
        if (nquad.getGraphName() != null) {
            write(nquad.getGraphName());
            writer.write(' ');            
        }
        
        writer.write(".\n");
    }

    public void write(RdfObject object) throws IOException {
        if (object == null) {
            throw new IllegalArgumentException();
        }
        
        if (object.isIRI()) {
            writeIri(object.toString());
            return;
        }
        
        if (object.isLiteral()) {
            write(object.getLiteral());
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
        
        if (literal.getLanguage() != null) {
            writer.write("@");
            writer.write(literal.getLanguage());
            
        } else if (literal.getDatatype() != null) {
            
            if (XsdVocabulary.STRING.equals(literal.getDatatype())) {
                return;
            }
            
            writer.write("^^");
            writeIri(literal.getDatatype());
        }
    }
    
    public static final String escape(String value) {
        
        final StringBuilder escaped = new StringBuilder();
        
        int[] codePoints = value.codePoints().toArray();
        
        for (int i=0; i < codePoints.length; i++) {
            
            int ch = codePoints[i]; 
            
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
                
            } else if (ch >= 0x0 && ch <= 0x1f || ch == 0x7f  /*|| Character.UnicodeBlock.of(ch) != Character.UnicodeBlock.BASIC_LATIN*/) {
     
                escaped.append(String.format ("\\u%04x", ch));
                
            } else {
                
                //TODO unicode
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

    public void write(RdfSubject subject) throws IOException {

        if (subject == null) {
            throw new IllegalArgumentException();
        }
            
        if (subject.isIRI()) {
            writeIri(subject.toString());
            return;
        }

        if (subject.isBlankNode()) {
            writer.write(subject.toString());
            return;
        }

        throw new IllegalStateException();
    }
    
    public void write(RdfPredicate subject) throws IOException {

        if (subject == null) {
            throw new IllegalArgumentException();
        }
            
        if (subject.isIRI()) {
            writeIri(subject.toString());
            return;
        }

        if (subject.isBlankNode()) {
            writer.write(subject.toString());
            return;
        }

        throw new IllegalStateException();
    }

    
    public void write(RdfGraphName graphName) throws IOException {

        if (graphName == null) {
            throw new IllegalArgumentException();
        }
            
        if (graphName.isIRI()) {
            writeIri(graphName.toString());
            return;
        }

        if (graphName.isBlankNode()) {
            writer.write(graphName.toString());
            return;
        }

        throw new IllegalStateException();
    }
}
