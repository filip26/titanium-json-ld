package com.apicatalog.rdf.api;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
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
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.io.RdfFormat;
import com.apicatalog.rdf.io.RdfReader;
import com.apicatalog.rdf.io.RdfWriter;
import com.apicatalog.rdf.spi.RdfProvider;

public final class Rdf {

    private Rdf() {
    }
    
    public static final RdfGraph createGraph() {
        return RdfProvider.provider().createGraph();
    }

    public static final RdfReader createReader(Reader reader, RdfFormat format) {
        return RdfProvider.provider().createReader(reader, format);
    }
    
    public static final RdfReader createReader(InputStream is, RdfFormat format) {
        return createReader(new InputStreamReader(is), format);
    }

    public static final RdfWriter createWriter(Writer writer, RdfFormat format) {
        return RdfProvider.provider().createWriter(writer, format);
    }

    public static final RdfWriter createWriter(OutputStream os, RdfFormat format) {
        return createWriter(new OutputStreamWriter(os), format);
    }

    public static final RdfDataset createDataset() {
        return RdfProvider.provider().createDataset();
    }

    public static final RdfStatementBuilder createStatementBuilder() {
        return new RdfStatementBuilder();
    }
    
    public static final RdfTriple createTriple(RdfSubject subject, RdfPredicate predicate, RdfObject object) {
        return RdfProvider.provider().createTriple(subject, predicate, object);
    }

    public static final RdfNQuad createNQuad(RdfSubject object, RdfPredicate predicate, RdfObject subject, RdfGraphName graphName) {
        return RdfProvider.provider().createNQuad(object, predicate, subject, graphName);
    }

    public static RdfSubject createSubject(RdfSubject.Type type, String value) {
        return RdfProvider.provider().createSubject(type, value);        
    }
    
    public static RdfPredicate createPredicate(RdfPredicate.Type type, String value) {
        return RdfProvider.provider().createPredicate(type, value);
    }

    public static RdfObject createObject(RdfObject.Type type, String value) {
        return RdfProvider.provider().createObject(type, value);
    }
    
    public static RdfObject createObject(RdfLiteral literal) {
        return RdfProvider.provider().createObject(literal);
    }
    
    public static RdfLiteral createTypedString(String lexicalForm, String dataType) {
        return RdfProvider.provider().createTypedString(lexicalForm, dataType);
    }
    
    public static RdfLiteral createLangString(String lexicalForm, String langTag) {
        return RdfProvider.provider().createLangString(lexicalForm, langTag);
    }

    public static RdfGraphName createGraphName(RdfGraphName.Type type, String value) {
        return RdfProvider.provider().createGraphName(type, value);
    }
}
