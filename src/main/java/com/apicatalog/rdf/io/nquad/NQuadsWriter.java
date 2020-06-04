package com.apicatalog.rdf.io.nquad;

import java.io.IOException;
import java.io.Writer;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.io.RdfWriter;

public class NQuadsWriter implements RdfWriter {

    private final Writer writer;
    
    public NQuadsWriter(Writer writer) {
        this.writer = writer;
    }

    @Override
    public void write(RdfDataset dataset) throws IOException {
        
        for (RdfNQuad nquad : dataset.toList()) {
            write(nquad);
        }

        writer.flush();
    }
    
    public void write(RdfNQuad nquad) throws IOException {
        
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
            write(object.asIRI());
            return;
        }
        
        if (object.isLiteral()) {
            write(object.asLiteral());
            return;
        }
        
        if (object.isBlankNode()) {
            write(object.asBlankNode());
            return;
        }

        throw new IllegalStateException();
    }

    private void write(BlankNode blankNode) throws IOException {
        writer.write(blankNode.toString());
    }

    private void write(RdfLiteral literal) throws IOException {
        writer.write('"');
        writer.write(escape(literal.getValue()));   //TODO unicode escape
        writer.write('"');
        
        if (literal.getLanguage() != null) {
            writer.write("@");
            writer.write(literal.getLanguage());
            
        } else if (literal.getDatatype() != null) {
            
            if ("http://www.w3.org/2001/XMLSchema#string".equals(literal.getDatatype().toString())) { //TODO constants!
                return;
            }
            
            writer.write("^^");
            write(literal.getDatatype());
        }
    }
    
    private static final String escape(String value) {
        
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
                escaped.append((char)ch);
            }
        }        
        return escaped.toString();
    }

    public void write(IRI iri) throws IOException {
        if (iri == null) {
            throw new IllegalArgumentException();
        }
        
        writer.write('<');
        writer.write(iri.toString());
        writer.write('>');
    }

    public void write(RdfSubject subject) throws IOException {

        if (subject == null) {
            throw new IllegalArgumentException();
        }
            
        if (subject.isIRI()) {
            write(subject.asIRI());
            return;
        }

        if (subject.isBlankNode()) {
            writer.write(subject.asBlankNode().toString());
            return;
        }

        throw new IllegalStateException();
    }
    
    public void write(RdfGraphName graphName) throws IOException {

        if (graphName == null) {
            throw new IllegalArgumentException();
        }
            
        if (graphName.isIRI()) {
            write(graphName.asIRI());
            return;
        }

        if (graphName.isBlankNode()) {
            writer.write(graphName.asBlankNode().toString());
            return;
        }

        throw new IllegalStateException();
    }
}