package com.apicatalog.rdf;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.lang.RdfConstants;
import com.apicatalog.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
    public static final RdfGraph createGraph() {
        return RdfProvider.provider().createGraph();
    }

    public static final RdfReader createReader(Reader reader, RdfFormat format) throws UnsupportedFormatException {

        if (reader == null || format == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createReader(reader, format);
    }
    
    public static final RdfReader createReader(InputStream is, RdfFormat format) throws UnsupportedFormatException {
        
        if (is == null || format == null) {
            throw new IllegalArgumentException();
        }

        return createReader(new InputStreamReader(is), format);
    }

    public static final RdfWriter createWriter(Writer writer, RdfFormat format) throws UnsupportedFormatException {
        
        if (writer == null || format == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createWriter(writer, format);
    }

    public static final RdfWriter createWriter(OutputStream os, RdfFormat format) throws UnsupportedFormatException {
        
        if (os == null || format == null) {
            throw new IllegalArgumentException();
        }

        return createWriter(new OutputStreamWriter(os), format);
    }

    public static final RdfDataset createDataset() {
        return RdfProvider.provider().createDataset();
    }

    public static final RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object) {
        
        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createTriple(subject, predicate, object);
    }

    public static final RdfNQuad createNQuad(RdfSubject object, RdfPredicate predicate, RdfObject subject, RdfGraphName graphName) {
        
        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createNQuad(object, predicate, subject, graphName);
    }

    public static RdfSubject createSubject(RdfSubject.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createSubject(type, value);        
    }
    
    public static RdfPredicate createPredicate(RdfPredicate.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createPredicate(type, value);
    }

    public static RdfObject createObject(RdfObject.Type type, String value) {
        
        if (type == null || value == null /*|| value.isBlank()*/) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createObject(type, value);
    }
    
    public static RdfObject createObject(RdfLiteral literal) {
        
        if (literal == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createObject(literal);
    }
    
    public static RdfLiteral createTypedString(String lexicalForm, String dataType) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createLiteral(lexicalForm, null, dataType);
    }
    
    public static RdfLiteral createLangString(String lexicalForm, String langTag) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return RdfProvider.provider().createLiteral(lexicalForm, langTag, RdfConstants.LANG_STRING);
    }

    public static RdfGraphName createGraphName(RdfGraphName.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }

        return RdfProvider.provider().createGraphName(type, value);
    }
}