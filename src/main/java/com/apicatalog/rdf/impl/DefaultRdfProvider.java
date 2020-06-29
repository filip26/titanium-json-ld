package com.apicatalog.rdf.impl;

import java.io.Reader;
import java.io.Writer;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfGraphName;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfSubject.Type;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.io.error.UnsupportedFormatException;
import com.apicatalog.rdf.io.nquad.NQuadsReader;
import com.apicatalog.rdf.io.nquad.NQuadsWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class DefaultRdfProvider extends RdfProvider {

    public static final RdfProvider INSTANCE = new DefaultRdfProvider(); 
    
    @Override
    public RdfDataset createDataset() {
        return new RdfDatasetImpl();
    }

    @Override
    public RdfReader createReader(Reader reader, RdfFormat format) throws UnsupportedFormatException {
        
        if (reader == null || format == null) {
            throw new IllegalArgumentException();
        }
        
        if (RdfFormat.N_QUADS.equals(format)) {
            return new NQuadsReader(reader);            
        }
        throw new UnsupportedFormatException(format);
    }

    @Override
    public RdfWriter createWriter(Writer writer, RdfFormat format) throws UnsupportedFormatException {

        if (writer == null || format == null) {
            throw new IllegalArgumentException();
        }

        if (RdfFormat.N_QUADS.equals(format)) {
            return new NQuadsWriter(writer);            
        }
        
        throw new UnsupportedFormatException(format);
    }

    @Override
    public RdfGraph createGraph() {
        return new RdfGraphImpl();
    }

    @Override
    public RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object) {
        
        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }

        return new RdfTripleImpl(subject, predicate, object);
    }

    @Override
    public RdfNQuad createNQuad(RdfSubject subject, RdfPredicate predicate, RdfObject object, RdfGraphName graphName) {
        
        if (subject == null || predicate == null || object == null) {
            throw new IllegalArgumentException();
        }
        
        return new RdfNQuadImpl(subject, predicate, object, graphName);
    }

    @Override
    public RdfSubject createSubject(Type type, String value) {
        
        if (type == null || value == null || isBlank(value)) {
            throw new IllegalArgumentException();
        }
                
        return new RdfSubjectImpl(type, value);
    }

    @Override
    public RdfPredicate createPredicate(RdfPredicate.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }
                
        return new RdfPredicateImpl(type, value);
    }

    @Override
    public RdfObject createObject(RdfObject.Type type, String value) {
        
        if (type == null || value == null || isBlank(value)) {
            throw new IllegalArgumentException();
        }
        
        return new RdfObjectImpl(type, value);
    }

    @Override
    public RdfLiteral createLiteral(String lexicalForm, String langTag, String dataType) {
        
        if (lexicalForm == null) {
            throw new IllegalArgumentException();
        }
        
        return new RdfLiteralImpl(lexicalForm, langTag, dataType);
    }

    @Override
    public RdfGraphName createGraphName(RdfGraphName.Type type, String value) {
        
        if (type == null || value == null || value.isBlank()) {
            throw new IllegalArgumentException();
        }

        return new RdfGraphNameImpl(type, value);
    }

    @Override
    public RdfObject createObject(RdfLiteral literal) {
        
        if (literal == null) {
            throw new IllegalArgumentException();
        }

        return new RdfObjectImpl(literal);
    }
    
    private static final boolean isBlank(String value) {
        return value.isEmpty() 
                || value.isBlank() && !value.chars().anyMatch(ch -> ch == '\n' || ch == '\r' || ch == '\t' || ch == '\f');
    }
}